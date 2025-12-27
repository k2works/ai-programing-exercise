using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// 作業指示 API の統合テスト
/// </summary>
[Collection("Postgres")]
public class WorkOrderControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public WorkOrderControllerTests(PostgresFixture fixture)
    {
        _fixture = fixture;
        _factory = new CustomWebApplicationFactory(fixture.ConnectionString);
        _client = _factory.CreateClient();
    }

    public Task InitializeAsync() => _fixture.CleanDatabaseAsync();

    public Task DisposeAsync()
    {
        _client.Dispose();
        _factory.Dispose();
        return Task.CompletedTask;
    }

    /// <summary>
    /// テストデータのセットアップ
    /// </summary>
    private async Task SetupMasterDataAsync()
    {
        // 品目マスタを作成
        await _client.PostAsJsonAsync("/api/items", new CreateItemRequest(
            ItemCode: "PROD-001",
            ItemName: "製品A",
            Category: "Product"
        ));

        // 場所マスタ、工程マスタ、工程表、オーダ情報を直接DBに作成
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();

        // 場所マスタ
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "場所マスタ" ("場所コード", "場所名", "場所区分")
                VALUES ('WH001', '資材倉庫1', '倉庫')
                ON CONFLICT ("場所コード") DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }

        // 工程マスタ
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "工程マスタ" ("工程コード", "工程名")
                VALUES ('ASSEMBLY', '組立工程')
                ON CONFLICT ("工程コード") DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }

        // 工程表
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "工程表" ("品目コード", "工順", "工程コード")
                VALUES ('PROD-001', 1, 'ASSEMBLY')
                ON CONFLICT DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }

        // オーダ情報（製造オーダ）
        await using (var cmd = conn.CreateCommand())
        {
            cmd.CommandText = """
                INSERT INTO "オーダ情報" (
                    "オーダNO", "オーダ種別", "品目コード", "着手予定日", "納期",
                    "計画数量", "場所コード", "ステータス"
                )
                VALUES (
                    'MO-001', '製造', 'PROD-001', '2025-01-21', '2025-01-25',
                    100, 'WH001', '確定'
                )
                ON CONFLICT ("オーダNO") DO NOTHING
            """;
            await cmd.ExecuteNonQueryAsync();
        }
    }

    [Fact]
    public async Task GetAllWorkOrders_EmptyDatabase_ReturnsEmptyList()
    {
        // Act
        var response = await _client.GetAsync("/api/work-orders");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var workOrders = await response.Content.ReadFromJsonAsync<List<WorkOrderResponse>>();
        workOrders.Should().NotBeNull();
        workOrders.Should().BeEmpty();
    }

    [Fact]
    public async Task CreateWorkOrder_ValidRequest_ReturnsCreated()
    {
        // Arrange
        await SetupMasterDataAsync();

        var request = new CreateWorkOrderRequest(
            OrderNumber: "MO-001",
            WorkOrderDate: new DateOnly(2025, 1, 20),
            LocationCode: "WH001",
            PlannedStartDate: new DateOnly(2025, 1, 21),
            PlannedEndDate: new DateOnly(2025, 1, 25),
            Remarks: "テスト作業指示"
        );

        // Act
        var response = await _client.PostAsJsonAsync("/api/work-orders", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var workOrder = await response.Content.ReadFromJsonAsync<WorkOrderResponse>();
        workOrder.Should().NotBeNull();
        workOrder!.OrderNumber.Should().Be("MO-001");
        workOrder.ItemCode.Should().Be("PROD-001");
        workOrder.OrderQuantity.Should().Be(100m);
        workOrder.Status.Should().Be("未着手");
        workOrder.Details.Should().HaveCount(1);
        workOrder.Details[0].ProcessCode.Should().Be("ASSEMBLY");
    }

    [Fact]
    public async Task GetWorkOrder_ExistingWorkOrder_ReturnsWorkOrder()
    {
        // Arrange
        await SetupMasterDataAsync();

        var createRequest = new CreateWorkOrderRequest(
            OrderNumber: "MO-001",
            WorkOrderDate: new DateOnly(2025, 1, 20),
            LocationCode: "WH001",
            PlannedStartDate: new DateOnly(2025, 1, 21),
            PlannedEndDate: new DateOnly(2025, 1, 25),
            Remarks: null
        );

        var createResponse = await _client.PostAsJsonAsync("/api/work-orders", createRequest);
        var createdWorkOrder = await createResponse.Content.ReadFromJsonAsync<WorkOrderResponse>();

        // Act
        var response = await _client.GetAsync($"/api/work-orders/{createdWorkOrder!.WorkOrderNumber}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var workOrder = await response.Content.ReadFromJsonAsync<WorkOrderResponse>();
        workOrder.Should().NotBeNull();
        workOrder!.WorkOrderNumber.Should().Be(createdWorkOrder.WorkOrderNumber);
    }

    [Fact]
    public async Task GetWorkOrder_NonExistingWorkOrder_ReturnsNotFound()
    {
        // Act
        var response = await _client.GetAsync("/api/work-orders/NOTEXIST-001");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task StartWork_ValidWorkOrder_ReturnsOk()
    {
        // Arrange
        await SetupMasterDataAsync();

        var createRequest = new CreateWorkOrderRequest(
            OrderNumber: "MO-001",
            WorkOrderDate: new DateOnly(2025, 1, 20),
            LocationCode: "WH001",
            PlannedStartDate: new DateOnly(2025, 1, 21),
            PlannedEndDate: new DateOnly(2025, 1, 25),
            Remarks: null
        );

        var createResponse = await _client.PostAsJsonAsync("/api/work-orders", createRequest);
        var createdWorkOrder = await createResponse.Content.ReadFromJsonAsync<WorkOrderResponse>();

        // Act
        var response = await _client.PostAsync(
            $"/api/work-orders/{createdWorkOrder!.WorkOrderNumber}/start",
            null
        );

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var workOrder = await response.Content.ReadFromJsonAsync<WorkOrderResponse>();
        workOrder.Should().NotBeNull();
        workOrder!.Status.Should().Be("作業中");
        workOrder.ActualStartDate.Should().NotBeNull();
    }

    [Fact]
    public async Task CompleteWork_StartedWorkOrder_ReturnsOk()
    {
        // Arrange
        await SetupMasterDataAsync();

        var createRequest = new CreateWorkOrderRequest(
            OrderNumber: "MO-001",
            WorkOrderDate: new DateOnly(2025, 1, 20),
            LocationCode: "WH001",
            PlannedStartDate: new DateOnly(2025, 1, 21),
            PlannedEndDate: new DateOnly(2025, 1, 25),
            Remarks: null
        );

        var createResponse = await _client.PostAsJsonAsync("/api/work-orders", createRequest);
        var createdWorkOrder = await createResponse.Content.ReadFromJsonAsync<WorkOrderResponse>();

        // 作業を開始
        await _client.PostAsync(
            $"/api/work-orders/{createdWorkOrder!.WorkOrderNumber}/start",
            null
        );

        // Act
        var response = await _client.PostAsync(
            $"/api/work-orders/{createdWorkOrder.WorkOrderNumber}/complete",
            null
        );

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var workOrder = await response.Content.ReadFromJsonAsync<WorkOrderResponse>();
        workOrder.Should().NotBeNull();
        workOrder!.Status.Should().Be("完了");
        workOrder.ActualEndDate.Should().NotBeNull();
    }

    [Fact]
    public async Task GetAllWorkOrders_AfterCreatingWorkOrders_ReturnsAllWorkOrders()
    {
        // Arrange
        await SetupMasterDataAsync();

        // 2件目のオーダを追加
        await using var conn = _fixture.CreateConnection();
        await conn.OpenAsync();
        await using var cmd = conn.CreateCommand();
        cmd.CommandText = """
            INSERT INTO "オーダ情報" (
                "オーダNO", "オーダ種別", "品目コード", "着手予定日", "納期",
                "計画数量", "場所コード", "ステータス"
            )
            VALUES (
                'MO-002', '製造', 'PROD-001', '2025-01-26', '2025-01-30',
                200, 'WH001', '確定'
            )
        """;
        await cmd.ExecuteNonQueryAsync();

        // 2件の作業指示を作成
        await _client.PostAsJsonAsync("/api/work-orders", new CreateWorkOrderRequest(
            OrderNumber: "MO-001",
            WorkOrderDate: new DateOnly(2025, 1, 20),
            LocationCode: "WH001",
            PlannedStartDate: new DateOnly(2025, 1, 21),
            PlannedEndDate: new DateOnly(2025, 1, 25),
            Remarks: null
        ));

        await _client.PostAsJsonAsync("/api/work-orders", new CreateWorkOrderRequest(
            OrderNumber: "MO-002",
            WorkOrderDate: new DateOnly(2025, 1, 25),
            LocationCode: "WH001",
            PlannedStartDate: new DateOnly(2025, 1, 26),
            PlannedEndDate: new DateOnly(2025, 1, 30),
            Remarks: null
        ));

        // Act
        var response = await _client.GetAsync("/api/work-orders");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var workOrders = await response.Content.ReadFromJsonAsync<List<WorkOrderResponse>>();
        workOrders.Should().HaveCount(2);
    }
}
