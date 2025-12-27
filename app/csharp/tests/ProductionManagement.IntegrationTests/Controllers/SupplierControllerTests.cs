using System.Net;
using System.Net.Http.Json;
using FluentAssertions;
using ProductionManagement.Infrastructure.Rest.Dto;
using ProductionManagement.IntegrationTests.TestSetup;

namespace ProductionManagement.IntegrationTests.Controllers;

/// <summary>
/// 取引先 API の統合テスト
/// </summary>
[Collection("Postgres")]
public class SupplierControllerTests : IAsyncLifetime
{
    private readonly PostgresFixture _fixture;
    private readonly CustomWebApplicationFactory _factory;
    private readonly HttpClient _client;

    public SupplierControllerTests(PostgresFixture fixture)
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

    [Fact]
    public async Task GetAllSuppliers_EmptyDatabase_ReturnsEmptyList()
    {
        // Act
        var response = await _client.GetAsync("/api/suppliers");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var suppliers = await response.Content.ReadFromJsonAsync<List<SupplierResponse>>();
        suppliers.Should().NotBeNull();
        suppliers.Should().BeEmpty();
    }

    [Fact]
    public async Task CreateSupplier_ValidRequest_ReturnsCreated()
    {
        // Arrange
        var request = new CreateSupplierRequest(
            SupplierCode: "SUP-001",
            SupplierName: "テスト取引先",
            SupplierType: "仕入先",
            SupplierNameKana: "テストトリヒキサキ",
            PostalCode: "100-0001",
            Address: "東京都千代田区",
            PhoneNumber: "03-1234-5678",
            FaxNumber: "03-1234-5679",
            ContactPerson: "山田太郎");

        // Act
        var response = await _client.PostAsJsonAsync("/api/suppliers", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var supplier = await response.Content.ReadFromJsonAsync<SupplierResponse>();
        supplier.Should().NotBeNull();
        supplier!.SupplierCode.Should().Be("SUP-001");
        supplier.SupplierName.Should().Be("テスト取引先");
        supplier.SupplierType.Should().Be("仕入先");
        supplier.SupplierNameKana.Should().Be("テストトリヒキサキ");
        supplier.PostalCode.Should().Be("100-0001");
        supplier.Address.Should().Be("東京都千代田区");
        supplier.PhoneNumber.Should().Be("03-1234-5678");
        supplier.FaxNumber.Should().Be("03-1234-5679");
        supplier.ContactPerson.Should().Be("山田太郎");
    }

    [Fact]
    public async Task GetSupplier_ExistingSupplier_ReturnsSupplier()
    {
        // Arrange
        var createRequest = new CreateSupplierRequest(
            SupplierCode: "SUP-002",
            SupplierName: "取引先2",
            SupplierType: "外注先");
        await _client.PostAsJsonAsync("/api/suppliers", createRequest);

        // Act
        var response = await _client.GetAsync("/api/suppliers/SUP-002");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var supplier = await response.Content.ReadFromJsonAsync<SupplierResponse>();
        supplier.Should().NotBeNull();
        supplier!.SupplierCode.Should().Be("SUP-002");
        supplier.SupplierName.Should().Be("取引先2");
        supplier.SupplierType.Should().Be("外注先");
    }

    [Fact]
    public async Task GetSupplier_NonExistingSupplier_ReturnsNotFound()
    {
        // Act
        var response = await _client.GetAsync("/api/suppliers/NOTEXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task CreateSupplier_DuplicateCode_ReturnsConflict()
    {
        // Arrange
        var request = new CreateSupplierRequest(
            SupplierCode: "SUP-DUP",
            SupplierName: "重複テスト",
            SupplierType: "仕入先");

        await _client.PostAsJsonAsync("/api/suppliers", request);

        // Act - 同じコードで2回目の作成
        var response = await _client.PostAsJsonAsync("/api/suppliers", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
    }

    [Fact]
    public async Task GetAllSuppliers_WithTypeFilter_ReturnsFilteredSuppliers()
    {
        // Arrange
        await _client.PostAsJsonAsync("/api/suppliers", new CreateSupplierRequest(
            SupplierCode: "SUP-A",
            SupplierName: "仕入先A",
            SupplierType: "仕入先"));

        await _client.PostAsJsonAsync("/api/suppliers", new CreateSupplierRequest(
            SupplierCode: "SUP-B",
            SupplierName: "外注先B",
            SupplierType: "外注先"));

        await _client.PostAsJsonAsync("/api/suppliers", new CreateSupplierRequest(
            SupplierCode: "SUP-C",
            SupplierName: "仕入先C",
            SupplierType: "仕入先"));

        // Act
        var response = await _client.GetAsync("/api/suppliers?type=仕入先");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var suppliers = await response.Content.ReadFromJsonAsync<List<SupplierResponse>>();
        suppliers.Should().HaveCount(2);
        suppliers.Should().OnlyContain(s => s.SupplierType == "仕入先");
    }

    [Fact]
    public async Task GetAllSuppliers_WithoutFilter_ReturnsAllSuppliers()
    {
        // Arrange
        await _client.PostAsJsonAsync("/api/suppliers", new CreateSupplierRequest(
            SupplierCode: "SUP-1",
            SupplierName: "取引先1",
            SupplierType: "仕入先"));

        await _client.PostAsJsonAsync("/api/suppliers", new CreateSupplierRequest(
            SupplierCode: "SUP-2",
            SupplierName: "取引先2",
            SupplierType: "外注先"));

        await _client.PostAsJsonAsync("/api/suppliers", new CreateSupplierRequest(
            SupplierCode: "SUP-3",
            SupplierName: "取引先3",
            SupplierType: "得意先"));

        // Act
        var response = await _client.GetAsync("/api/suppliers");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);
        var suppliers = await response.Content.ReadFromJsonAsync<List<SupplierResponse>>();
        suppliers.Should().HaveCount(3);
    }
}
