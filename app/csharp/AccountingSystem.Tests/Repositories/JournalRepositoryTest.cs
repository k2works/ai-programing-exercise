using AccountingSystem.Infrastructure.Entities;
using AccountingSystem.Infrastructure.Repositories;
using FluentAssertions;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Repositories;

/// <summary>
/// 3層構造仕訳リポジトリ - 統合テスト
/// 2.4節: TDDサイクルで実装
/// </summary>
public class JournalRepositoryTest : IAsyncLifetime
{
    private readonly PostgreSqlContainer _postgres;
    private TestDatabase? _testDb;
    private JournalRepository? _repository;

    public JournalRepositoryTest()
    {
        _postgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("testdb")
            .WithUsername("testuser")
            .WithPassword("testpass")
            .Build();
    }

    public async Task InitializeAsync()
    {
        await _postgres.StartAsync();
        _testDb = new TestDatabase(_postgres);
        await _testDb.StartAsync();
        _repository = new JournalRepository(_postgres.GetConnectionString());
    }

    public async Task DisposeAsync()
    {
        await _testDb!.StopAsync();
        await _postgres.DisposeAsync();
    }

    private async Task CleanupAsync()
    {
        await using var conn = new NpgsqlConnection(_postgres.GetConnectionString());
        await conn.OpenAsync();

        // CASCADE により子テーブルも削除される
        await using var cmd = new NpgsqlCommand(@"TRUNCATE TABLE ""仕訳"" CASCADE", conn);
        await cmd.ExecuteNonQueryAsync();
    }

    private async Task InsertTestAccountAsync(string accountCode, string accountName)
    {
        await using var conn = new NpgsqlConnection(_postgres.GetConnectionString());
        await conn.OpenAsync();

        await using var cmd = new NpgsqlCommand(@"
            INSERT INTO ""勘定科目マスタ"" (""勘定科目コード"", ""勘定科目名"", ""勘定科目種別"", ""合計科目"")
            VALUES (@code, @name, '資産', false)
            ON CONFLICT (""勘定科目コード"") DO NOTHING
        ", conn);
        cmd.Parameters.AddWithValue("code", accountCode);
        cmd.Parameters.AddWithValue("name", accountName);
        await cmd.ExecuteNonQueryAsync();
    }

    [Fact(DisplayName = "3層構造での仕訳登録_単純な仕訳")]
    public async Task TestInsert_SimpleJournal()
    {
        await CleanupAsync();

        // 勘定科目マスタにテストデータを登録
        await InsertTestAccountAsync("1110", "現金");
        await InsertTestAccountAsync("4110", "売上高");

        var journal = new Journal
        {
            JournalNo = "J2025-0001",
            JournalDate = new DateOnly(2025, 1, 21),
            InputDate = new DateOnly(2025, 1, 21),
            SettlementFlag = 0,
            SingleEntryFlag = 1,
            JournalType = 0,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetail>
            {
                new JournalDetail
                {
                    JournalNo = "J2025-0001",
                    LineNumber = 1,
                    Description = "商品売上",
                    Items = new List<JournalDetailItem>
                    {
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0001",
                            LineNumber = 1,
                            DebitCreditFlag = "D",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "1110",
                            Amount = 10000.00m,
                            BaseAmount = 10000.00m,
                            CashFlowFlag = 1
                        },
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0001",
                            LineNumber = 1,
                            DebitCreditFlag = "C",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "4110",
                            Amount = 10000.00m,
                            BaseAmount = 10000.00m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };

        await _repository!.InsertAsync(journal);

        var found = await _repository.FindByJournalNoAsync("J2025-0001");
        found.Should().NotBeNull();
        found!.JournalNo.Should().Be("J2025-0001");
        found.Details.Should().HaveCount(1);
        found.Details[0].Items.Should().HaveCount(2);
        found.Details[0].Description.Should().Be("商品売上");
    }

    [Fact(DisplayName = "3層構造での仕訳登録_複合仕訳")]
    public async Task TestInsert_CompoundJournal()
    {
        await CleanupAsync();

        // 勘定科目マスタにテストデータを登録
        await InsertTestAccountAsync("1110", "現金");
        await InsertTestAccountAsync("1120", "売掛金");
        await InsertTestAccountAsync("4110", "売上高");

        var journal = new Journal
        {
            JournalNo = "J2025-0002",
            JournalDate = new DateOnly(2025, 1, 21),
            InputDate = new DateOnly(2025, 1, 21),
            SettlementFlag = 0,
            SingleEntryFlag = 0, // 複合仕訳
            JournalType = 0,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetail>
            {
                new JournalDetail
                {
                    JournalNo = "J2025-0002",
                    LineNumber = 1,
                    Description = "現金売上",
                    Items = new List<JournalDetailItem>
                    {
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0002",
                            LineNumber = 1,
                            DebitCreditFlag = "D",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "1110",
                            Amount = 5000.00m,
                            BaseAmount = 5000.00m,
                            CashFlowFlag = 1
                        }
                    }
                },
                new JournalDetail
                {
                    JournalNo = "J2025-0002",
                    LineNumber = 2,
                    Description = "掛売上",
                    Items = new List<JournalDetailItem>
                    {
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0002",
                            LineNumber = 2,
                            DebitCreditFlag = "D",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "1120",
                            Amount = 15000.00m,
                            BaseAmount = 15000.00m,
                            CashFlowFlag = 0
                        }
                    }
                },
                new JournalDetail
                {
                    JournalNo = "J2025-0002",
                    LineNumber = 3,
                    Description = "売上計上",
                    Items = new List<JournalDetailItem>
                    {
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0002",
                            LineNumber = 3,
                            DebitCreditFlag = "C",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "4110",
                            Amount = 20000.00m,
                            BaseAmount = 20000.00m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };

        await _repository!.InsertAsync(journal);

        var found = await _repository.FindByJournalNoAsync("J2025-0002");
        found.Should().NotBeNull();
        found!.SingleEntryFlag.Should().Be(0);
        found.Details.Should().HaveCount(3);
    }

    [Fact(DisplayName = "外部キー制約_仕訳削除時に明細も削除される")]
    public async Task TestCascadeDelete()
    {
        await CleanupAsync();

        await InsertTestAccountAsync("1110", "現金");
        await InsertTestAccountAsync("4110", "売上高");

        var journal = new Journal
        {
            JournalNo = "J2025-0003",
            JournalDate = new DateOnly(2025, 1, 21),
            InputDate = new DateOnly(2025, 1, 21),
            SettlementFlag = 0,
            SingleEntryFlag = 1,
            JournalType = 0,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetail>
            {
                new JournalDetail
                {
                    JournalNo = "J2025-0003",
                    LineNumber = 1,
                    Description = "テスト仕訳",
                    Items = new List<JournalDetailItem>
                    {
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0003",
                            LineNumber = 1,
                            DebitCreditFlag = "D",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "1110",
                            Amount = 1000.00m,
                            BaseAmount = 1000.00m,
                            CashFlowFlag = 1
                        },
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0003",
                            LineNumber = 1,
                            DebitCreditFlag = "C",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "4110",
                            Amount = 1000.00m,
                            BaseAmount = 1000.00m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };

        await _repository!.InsertAsync(journal);

        // 仕訳を削除
        await _repository.DeleteByJournalNoAsync("J2025-0003");

        // CASCADE により明細も削除されていることを確認
        var found = await _repository.FindByJournalNoAsync("J2025-0003");
        found.Should().BeNull();
    }

    [Fact(DisplayName = "複式簿記の原理_借方合計と貸方合計が一致")]
    public async Task TestDoubleEntryBalance()
    {
        await CleanupAsync();

        await InsertTestAccountAsync("1110", "現金");
        await InsertTestAccountAsync("4110", "売上高");

        var journal = new Journal
        {
            JournalNo = "J2025-0004",
            JournalDate = new DateOnly(2025, 1, 21),
            InputDate = new DateOnly(2025, 1, 21),
            SettlementFlag = 0,
            SingleEntryFlag = 1,
            JournalType = 0,
            RecurringFlag = 0,
            RedSlipFlag = 0,
            Details = new List<JournalDetail>
            {
                new JournalDetail
                {
                    JournalNo = "J2025-0004",
                    LineNumber = 1,
                    Description = "残高確認テスト",
                    Items = new List<JournalDetailItem>
                    {
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0004",
                            LineNumber = 1,
                            DebitCreditFlag = "D",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "1110",
                            Amount = 50000.00m,
                            BaseAmount = 50000.00m,
                            CashFlowFlag = 1
                        },
                        new JournalDetailItem
                        {
                            JournalNo = "J2025-0004",
                            LineNumber = 1,
                            DebitCreditFlag = "C",
                            CurrencyCode = "JPY",
                            ExchangeRate = 1.0000m,
                            AccountCode = "4110",
                            Amount = 50000.00m,
                            BaseAmount = 50000.00m,
                            CashFlowFlag = 0
                        }
                    }
                }
            }
        };

        await _repository!.InsertAsync(journal);

        var (debitTotal, creditTotal) = await _repository.GetBalanceAsync("J2025-0004");

        // 複式簿記の原理: 借方合計 = 貸方合計
        debitTotal.Should().Be(50000.00m);
        creditTotal.Should().Be(50000.00m);
        debitTotal.Should().Be(creditTotal);
    }
}
