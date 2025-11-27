using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Infrastructure.Web.Controllers;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// イベントソーシング版仕訳 API（イベントバス連携）統合テスト
/// </summary>
public class JournalEntryEventSourcingWithEventBusApiTest : ApiTestBase
{
    private const string BaseUrl = "/api/v1/journal-entries-es-eventbus";

    private static CreateJournalEntryRequest CreateValidRequest()
    {
        return new CreateJournalEntryRequest
        {
            EntryDate = new DateOnly(2025, 1, 15),
            Description = "テスト仕訳（EventBus連携）",
            UserId = "user1",
            LineItems = new List<LineItemRequest>
            {
                new() { AccountCode = "1000", DebitCredit = "DEBIT", Amount = 10000m },
                new() { AccountCode = "2000", DebitCredit = "CREDIT", Amount = 10000m }
            }
        };
    }

    [Fact(DisplayName = "POST /api/v1/journal-entries-es-eventbus - 仕訳を作成できる")]
    public async Task CreateJournalEntry_Returns201()
    {
        // Arrange
        var request = CreateValidRequest();

        // Act
        var response = await Client.PostAsJsonAsync(BaseUrl, request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var created = await response.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();
        created.Should().NotBeNull();
        created!.Id.Should().NotBeNullOrEmpty();
    }

    [Fact(DisplayName = "POST /api/v1/journal-entries-es-eventbus - 貸借不一致で400を返す")]
    public async Task CreateJournalEntry_UnbalancedEntry_Returns400()
    {
        // Arrange
        var request = new CreateJournalEntryRequest
        {
            EntryDate = new DateOnly(2025, 1, 15),
            Description = "不正な仕訳",
            UserId = "user1",
            LineItems = new List<LineItemRequest>
            {
                new() { AccountCode = "1000", DebitCredit = "DEBIT", Amount = 10000m },
                new() { AccountCode = "2000", DebitCredit = "CREDIT", Amount = 5000m } // 貸借不一致
            }
        };

        // Act
        var response = await Client.PostAsJsonAsync(BaseUrl, request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact(DisplayName = "GET /api/v1/journal-entries-es-eventbus/{id} - 仕訳を取得できる")]
    public async Task GetJournalEntry_Returns200()
    {
        // Arrange
        var request = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, request);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        // Act
        var response = await Client.GetAsync($"{BaseUrl}/{created!.Id}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var journal = await response.Content.ReadFromJsonAsync<JournalEntryResponse>();
        journal.Should().NotBeNull();
        journal!.Id.Should().Be(created.Id);
        journal.EntryDate.Should().Be(new DateOnly(2025, 1, 15));
        journal.Description.Should().Be("テスト仕訳（EventBus連携）");
        journal.Status.Should().Be("DRAFT");
        journal.Deleted.Should().BeFalse();
        journal.LineItems.Should().HaveCount(2);
    }

    [Fact(DisplayName = "GET /api/v1/journal-entries-es-eventbus/{id} - 存在しないIDで404を返す")]
    public async Task GetJournalEntry_NotFound_Returns404()
    {
        // Act
        var response = await Client.GetAsync($"{BaseUrl}/non-existent-id");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "POST /api/v1/journal-entries-es-eventbus/{id}/approve - 仕訳を承認できる")]
    public async Task ApproveJournalEntry_Returns204()
    {
        // Arrange
        var createRequest = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, createRequest);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        var approveRequest = new ApproveJournalEntryRequest
        {
            ApprovedBy = "approver1",
            Comment = "承認します"
        };

        // Act
        var response = await Client.PostAsJsonAsync(
            $"{BaseUrl}/{created!.Id}/approve", approveRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 状態確認
        var getResponse = await Client.GetAsync($"{BaseUrl}/{created.Id}");
        var journal = await getResponse.Content.ReadFromJsonAsync<JournalEntryResponse>();
        journal!.Status.Should().Be("APPROVED");
    }

    [Fact(DisplayName = "POST /api/v1/journal-entries-es-eventbus/{id}/approve - 存在しないIDで404を返す")]
    public async Task ApproveJournalEntry_NotFound_Returns404()
    {
        // Arrange
        var approveRequest = new ApproveJournalEntryRequest
        {
            ApprovedBy = "approver1",
            Comment = "承認します"
        };

        // Act
        var response = await Client.PostAsJsonAsync(
            $"{BaseUrl}/non-existent-id/approve", approveRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "POST /api/v1/journal-entries-es-eventbus/{id}/approve - 既に承認済みで400を返す")]
    public async Task ApproveJournalEntry_AlreadyApproved_Returns400()
    {
        // Arrange
        var createRequest = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, createRequest);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        var approveRequest = new ApproveJournalEntryRequest
        {
            ApprovedBy = "approver1",
            Comment = "承認します"
        };

        // 最初の承認
        await Client.PostAsJsonAsync(
            $"{BaseUrl}/{created!.Id}/approve", approveRequest);

        // Act: 再度承認
        var response = await Client.PostAsJsonAsync(
            $"{BaseUrl}/{created.Id}/approve", approveRequest);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact(DisplayName = "DELETE /api/v1/journal-entries-es-eventbus/{id} - 仕訳を削除できる")]
    public async Task DeleteJournalEntry_Returns204()
    {
        // Arrange
        var createRequest = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, createRequest);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        var deleteRequest = new DeleteJournalEntryRequest
        {
            Reason = "誤入力のため",
            UserId = "user1"
        };

        // Act
        var response = await Client.SendAsync(new HttpRequestMessage(HttpMethod.Delete,
            $"{BaseUrl}/{created!.Id}")
        {
            Content = JsonContent.Create(deleteRequest)
        });

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 削除確認（論理削除なので取得はできる）
        var getResponse = await Client.GetAsync($"{BaseUrl}/{created.Id}");
        var journal = await getResponse.Content.ReadFromJsonAsync<JournalEntryResponse>();
        journal!.Deleted.Should().BeTrue();
    }

    [Fact(DisplayName = "DELETE /api/v1/journal-entries-es-eventbus/{id} - 存在しないIDで404を返す")]
    public async Task DeleteJournalEntry_NotFound_Returns404()
    {
        // Arrange
        var deleteRequest = new DeleteJournalEntryRequest
        {
            Reason = "誤入力のため",
            UserId = "user1"
        };

        // Act
        var response = await Client.SendAsync(new HttpRequestMessage(HttpMethod.Delete,
            $"{BaseUrl}/non-existent-id")
        {
            Content = JsonContent.Create(deleteRequest)
        });

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "GET /api/v1/journal-entries-es-eventbus/{id}/at - 特定時点の仕訳状態を取得できる")]
    public async Task GetJournalEntryAt_Returns200()
    {
        // Arrange
        var createRequest = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, createRequest);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        // 作成直後の時点を記録
        var afterCreation = DateTime.UtcNow;
        await Task.Delay(100); // 時間差を確保

        // 承認
        var approveRequest = new ApproveJournalEntryRequest
        {
            ApprovedBy = "approver1",
            Comment = "承認します"
        };
        await Client.PostAsJsonAsync(
            $"{BaseUrl}/{created!.Id}/approve", approveRequest);

        // Act: 承認前の時点を指定
        var response = await Client.GetAsync(
            $"{BaseUrl}/{created.Id}/at?pointInTime={afterCreation:o}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var journal = await response.Content.ReadFromJsonAsync<JournalEntryResponse>();
        journal!.Status.Should().Be("DRAFT"); // 承認前の状態
    }

    [Fact(DisplayName = "GET /api/v1/journal-entries-es-eventbus/{id}/at - 指定時点に存在しない場合404を返す")]
    public async Task GetJournalEntryAt_NotFound_Returns404()
    {
        // Arrange
        var beforeCreation = DateTime.UtcNow;
        await Task.Delay(100);

        var createRequest = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, createRequest);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        // Act: 作成前の時点を指定
        var response = await Client.GetAsync(
            $"{BaseUrl}/{created!.Id}/at?pointInTime={beforeCreation:o}");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "複合操作: 作成 → 承認 → 削除（EventBus連携）")]
    public async Task ComplexOperations_CreateApproveDelete()
    {
        // Arrange & Act
        // 1. 作成
        var createRequest = CreateValidRequest();
        var createResponse = await Client.PostAsJsonAsync(BaseUrl, createRequest);
        createResponse.StatusCode.Should().Be(HttpStatusCode.Created);
        var created = await createResponse.Content.ReadFromJsonAsync<CreateJournalEntryResponse>();

        // 2. 承認
        var approveRequest = new ApproveJournalEntryRequest
        {
            ApprovedBy = "approver1",
            Comment = "承認します"
        };
        var approveResponse = await Client.PostAsJsonAsync(
            $"{BaseUrl}/{created!.Id}/approve", approveRequest);
        approveResponse.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 3. 削除
        var deleteRequest = new DeleteJournalEntryRequest
        {
            Reason = "取り消しのため",
            UserId = "user1"
        };
        var deleteResponse = await Client.SendAsync(new HttpRequestMessage(HttpMethod.Delete,
            $"{BaseUrl}/{created.Id}")
        {
            Content = JsonContent.Create(deleteRequest)
        });
        deleteResponse.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // Assert: 最終状態の確認
        var getResponse = await Client.GetAsync($"{BaseUrl}/{created.Id}");
        var journal = await getResponse.Content.ReadFromJsonAsync<JournalEntryResponse>();
        journal!.Status.Should().Be("APPROVED");
        journal.Deleted.Should().BeTrue();
    }
}
