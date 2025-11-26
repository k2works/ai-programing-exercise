using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Infrastructure.Web.Dtos;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// 仕訳 API 統合テスト
/// </summary>
public class JournalApiTest : ApiTestBase
{
    protected override async Task OnInitializedAsync()
    {
        // 仕訳で使用する勘定科目をセットアップ
        await ExecuteSqlAsync(@"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目""
            ) VALUES
            ('1110', '普通預金', '資産', false),
            ('4110', '売上高', '収益', false)
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ");
    }

    private static JournalRequest CreateValidJournalRequest(string journalNo)
    {
        return new JournalRequest
        {
            JournalNo = journalNo,
            JournalDate = new DateOnly(2024, 1, 15),
            InputDate = new DateOnly(2024, 1, 15),
            SettlementFlag = 0,
            SingleEntryFlag = 0,
            JournalType = 1,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetailRequest>
            {
                new()
                {
                    LineNumber = 1,
                    Description = "売上取引",
                    Items = new List<JournalDetailItemRequest>
                    {
                        new()
                        {
                            DebitCreditFlag = "D",
                            AccountCode = "1110",
                            Amount = 100000m,
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        },
                        new()
                        {
                            DebitCreditFlag = "C",
                            AccountCode = "4110",
                            Amount = 100000m,
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };
    }

    [Fact(DisplayName = "POST /api/v1/journals - 仕訳を作成できる")]
    public async Task CreateJournal_Returns201()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115001");

        // Act
        var response = await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Created);
        response.Headers.Location.Should().NotBeNull();

        var created = await response.Content.ReadFromJsonAsync<JournalResponse>();
        created.Should().NotBeNull();
        created!.JournalNo.Should().Be("J20240115001");
        created.Details.Should().HaveCount(1);
        created.Details[0].Items.Should().HaveCount(2);
    }

    [Fact(DisplayName = "POST /api/v1/journals - 貸借不一致で400を返す")]
    public async Task CreateJournal_UnbalancedEntry_Returns400()
    {
        // Arrange
        var request = new JournalRequest
        {
            JournalNo = "J20240115002",
            JournalDate = new DateOnly(2024, 1, 15),
            InputDate = new DateOnly(2024, 1, 15),
            SettlementFlag = 0,
            SingleEntryFlag = 0,
            JournalType = 1,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetailRequest>
            {
                new()
                {
                    LineNumber = 1,
                    Description = "不正な仕訳",
                    Items = new List<JournalDetailItemRequest>
                    {
                        new()
                        {
                            DebitCreditFlag = "D",
                            AccountCode = "1110",
                            Amount = 100000m,
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        },
                        new()
                        {
                            DebitCreditFlag = "C",
                            AccountCode = "4110",
                            Amount = 50000m, // 貸借不一致
                            CurrencyCode = "JPY",
                            ExchangeRate = 1m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };

        // Act
        var response = await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact(DisplayName = "POST /api/v1/journals - 重複する伝票番号で409を返す")]
    public async Task CreateJournal_Duplicate_Returns409()
    {
        // Arrange - 最初の仕訳を作成
        var request = CreateValidJournalRequest("J20240115003");
        await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Act - 同じ伝票番号で再度作成
        var response = await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.Conflict);
    }

    [Fact(DisplayName = "GET /api/v1/journals/{journalNo} - 仕訳を取得できる")]
    public async Task GetJournal_Returns200()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115004");
        await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Act
        var response = await Client.GetAsync("/api/v1/journals/J20240115004");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var journal = await response.Content.ReadFromJsonAsync<JournalResponse>();
        journal.Should().NotBeNull();
        journal!.JournalNo.Should().Be("J20240115004");
        journal.JournalDate.Should().Be(new DateOnly(2024, 1, 15));
    }

    [Fact(DisplayName = "GET /api/v1/journals/{journalNo} - 存在しない伝票番号で404を返す")]
    public async Task GetJournal_NotFound_Returns404()
    {
        // Act
        var response = await Client.GetAsync("/api/v1/journals/NOTEXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "GET /api/v1/journals/{journalNo}/balance - 残高検証ができる")]
    public async Task ValidateBalance_Returns200()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115005");
        await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Act
        var response = await Client.GetAsync("/api/v1/journals/J20240115005/balance");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var balance = await response.Content.ReadFromJsonAsync<JournalBalanceResponse>();
        balance.Should().NotBeNull();
        balance!.JournalNo.Should().Be("J20240115005");
        balance.DebitTotal.Should().Be(100000m);
        balance.CreditTotal.Should().Be(100000m);
        balance.IsBalanced.Should().BeTrue();
    }

    [Fact(DisplayName = "DELETE /api/v1/journals/{journalNo} - 仕訳を削除できる")]
    public async Task DeleteJournal_Returns204()
    {
        // Arrange
        var request = CreateValidJournalRequest("J20240115006");
        await Client.PostAsJsonAsync("/api/v1/journals", request);

        // Act
        var response = await Client.DeleteAsync("/api/v1/journals/J20240115006");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NoContent);

        // 削除確認
        var getResponse = await Client.GetAsync("/api/v1/journals/J20240115006");
        getResponse.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact(DisplayName = "DELETE /api/v1/journals/{journalNo} - 存在しない伝票番号で404を返す")]
    public async Task DeleteJournal_NotFound_Returns404()
    {
        // Act
        var response = await Client.DeleteAsync("/api/v1/journals/NOTEXIST");

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }
}
