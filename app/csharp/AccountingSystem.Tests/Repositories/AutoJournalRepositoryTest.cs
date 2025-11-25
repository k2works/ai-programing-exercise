using AccountingSystem.Infrastructure.Entities;
using AccountingSystem.Infrastructure.Repositories;
using FluentAssertions;
using Npgsql;
using Testcontainers.PostgreSql;
using Xunit;

namespace AccountingSystem.Tests.Repositories;

/// <summary>
/// 自動仕訳リポジトリ - 統合テスト
/// </summary>
public class AutoJournalRepositoryTest : IAsyncLifetime
{
    private readonly PostgreSqlContainer _postgres;
    private TestDatabase? _testDb;
    private AutoJournalRepository? _repository;

    public AutoJournalRepositoryTest()
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
        _repository = new AutoJournalRepository(_postgres.GetConnectionString());
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

        await using var cmd1 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳実行ログ"" CASCADE", conn);
        await cmd1.ExecuteNonQueryAsync();

        await using var cmd2 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳パターン明細"" CASCADE", conn);
        await cmd2.ExecuteNonQueryAsync();

        await using var cmd3 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳パターン"" CASCADE", conn);
        await cmd3.ExecuteNonQueryAsync();

        await using var cmd4 = new NpgsqlCommand(@"TRUNCATE TABLE ""自動仕訳管理"" CASCADE", conn);
        await cmd4.ExecuteNonQueryAsync();
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

    #region 自動仕訳管理テスト

    [Fact(DisplayName = "自動仕訳管理_登録と取得ができる")]
    public async Task TestManagement_InsertAndFind()
    {
        await CleanupAsync();

        var management = new AutoJournalManagement
        {
            SourceTableName = "売上データ",
            LastProcessedAt = new DateTime(2025, 1, 21, 10, 0, 0)
        };

        var id = await _repository!.InsertManagementAsync(management);
        id.Should().BeGreaterThan(0);

        var found = await _repository.FindManagementBySourceTableAsync("売上データ");
        found.Should().NotBeNull();
        found!.SourceTableName.Should().Be("売上データ");
        found.LastProcessedAt.Should().Be(new DateTime(2025, 1, 21, 10, 0, 0));
    }

    [Fact(DisplayName = "自動仕訳管理_最終処理日時を更新できる")]
    public async Task TestManagement_UpdateLastProcessedAt()
    {
        await CleanupAsync();

        var management = new AutoJournalManagement
        {
            SourceTableName = "給与データ",
            LastProcessedAt = new DateTime(2025, 1, 1, 0, 0, 0)
        };

        var id = await _repository!.InsertManagementAsync(management);

        var newProcessedAt = new DateTime(2025, 1, 21, 15, 30, 0);
        await _repository.UpdateLastProcessedAtAsync(id, newProcessedAt);

        var found = await _repository.FindManagementBySourceTableAsync("給与データ");
        found!.LastProcessedAt.Should().Be(newProcessedAt);
    }

    #endregion

    #region 自動仕訳パターンテスト

    [Fact(DisplayName = "自動仕訳パターン_明細を含めて登録・取得できる")]
    public async Task TestPattern_InsertAndFindWithDetails()
    {
        await CleanupAsync();
        await InsertTestAccountAsync("1300", "売掛金");
        await InsertTestAccountAsync("4100", "売上");
        await InsertTestAccountAsync("2120", "仮受消費税");

        var pattern = new AutoJournalPattern
        {
            PatternCode = "SALES_001",
            PatternName = "売上仕訳パターン",
            SourceTableName = "売上データ",
            Description = "売上データから仕訳を自動生成",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0, // 登録時に設定される
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1300",
                    AmountExpression = "税込金額",
                    DescriptionTemplate = "売上: {顧客名}"
                },
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 2,
                    DebitCreditFlag = "C",
                    AccountCode = "4100",
                    AmountExpression = "税抜金額",
                    DescriptionTemplate = "売上: {顧客名}"
                },
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 3,
                    DebitCreditFlag = "C",
                    AccountCode = "2120",
                    AmountExpression = "消費税額",
                    DescriptionTemplate = "消費税: {顧客名}"
                }
            }
        };

        var patternId = await _repository!.InsertPatternAsync(pattern);
        patternId.Should().BeGreaterThan(0);

        var found = await _repository.FindPatternByIdAsync(patternId);
        found.Should().NotBeNull();
        found!.PatternCode.Should().Be("SALES_001");
        found.PatternName.Should().Be("売上仕訳パターン");
        found.IsActive.Should().BeTrue();
        found.Details.Should().HaveCount(3);
        found.Details[0].DebitCreditFlag.Should().Be("D");
        found.Details[1].DebitCreditFlag.Should().Be("C");
    }

    [Fact(DisplayName = "自動仕訳パターン_パターンコードで取得できる")]
    public async Task TestPattern_FindByCode()
    {
        await CleanupAsync();
        await InsertTestAccountAsync("1100", "現金");

        var pattern = new AutoJournalPattern
        {
            PatternCode = "CASH_001",
            PatternName = "現金仕訳パターン",
            SourceTableName = "現金データ",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        };

        await _repository!.InsertPatternAsync(pattern);

        var found = await _repository.FindPatternByCodeAsync("CASH_001");
        found.Should().NotBeNull();
        found!.PatternName.Should().Be("現金仕訳パターン");
    }

    [Fact(DisplayName = "自動仕訳パターン_有効なパターン一覧を取得できる")]
    public async Task TestPattern_FindActivePatterns()
    {
        await CleanupAsync();
        await InsertTestAccountAsync("1100", "現金");

        // 有効なパターン
        await _repository!.InsertPatternAsync(new AutoJournalPattern
        {
            PatternCode = "ACTIVE_001",
            PatternName = "有効パターン1",
            SourceTableName = "テスト",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        });

        await _repository.InsertPatternAsync(new AutoJournalPattern
        {
            PatternCode = "ACTIVE_002",
            PatternName = "有効パターン2",
            SourceTableName = "テスト",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        });

        // 無効なパターン
        await _repository.InsertPatternAsync(new AutoJournalPattern
        {
            PatternCode = "INACTIVE_001",
            PatternName = "無効パターン",
            SourceTableName = "テスト",
            IsActive = false,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        });

        var activePatterns = (await _repository.FindActivePattersAsync()).ToList();
        activePatterns.Should().HaveCount(2);
        activePatterns.Should().OnlyContain(p => p.IsActive);
    }

    [Fact(DisplayName = "自動仕訳パターン_削除時に明細とログもカスケード削除される")]
    public async Task TestPattern_CascadeDelete()
    {
        await CleanupAsync();
        await InsertTestAccountAsync("1100", "現金");

        var pattern = new AutoJournalPattern
        {
            PatternCode = "DELETE_TEST",
            PatternName = "削除テスト",
            SourceTableName = "テスト",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        };

        var patternId = await _repository!.InsertPatternAsync(pattern);

        // 実行ログを追加
        await _repository.InsertExecutionLogAsync(new AutoJournalExecutionLog
        {
            AutoJournalPatternId = patternId,
            ExecutedAt = DateTime.Now,
            ProcessedCount = 10,
            GeneratedCount = 10,
            Status = "SUCCESS"
        });

        // パターン削除
        await _repository.DeletePatternAsync(patternId);

        // 削除確認
        var found = await _repository.FindPatternByIdAsync(patternId);
        found.Should().BeNull();

        var logs = await _repository.FindExecutionLogsByPatternIdAsync(patternId);
        logs.Should().BeEmpty();
    }

    #endregion

    #region 自動仕訳実行ログテスト

    [Fact(DisplayName = "自動仕訳実行ログ_登録と取得ができる")]
    public async Task TestExecutionLog_InsertAndFind()
    {
        await CleanupAsync();
        await InsertTestAccountAsync("1100", "現金");

        var patternId = await _repository!.InsertPatternAsync(new AutoJournalPattern
        {
            PatternCode = "LOG_TEST",
            PatternName = "ログテスト",
            SourceTableName = "テスト",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        });

        var log = new AutoJournalExecutionLog
        {
            AutoJournalPatternId = patternId,
            ExecutedAt = new DateTime(2025, 1, 21, 10, 0, 0),
            ProcessedCount = 100,
            GeneratedCount = 98,
            Status = "SUCCESS",
            Message = "正常終了"
        };

        var logId = await _repository.InsertExecutionLogAsync(log);
        logId.Should().BeGreaterThan(0);

        var logs = (await _repository.FindExecutionLogsByPatternIdAsync(patternId)).ToList();
        logs.Should().HaveCount(1);
        logs[0].ProcessedCount.Should().Be(100);
        logs[0].GeneratedCount.Should().Be(98);
        logs[0].Status.Should().Be("SUCCESS");
    }

    [Fact(DisplayName = "自動仕訳実行ログ_最新のログを取得できる")]
    public async Task TestExecutionLog_FindLatest()
    {
        await CleanupAsync();
        await InsertTestAccountAsync("1100", "現金");

        var patternId = await _repository!.InsertPatternAsync(new AutoJournalPattern
        {
            PatternCode = "LATEST_TEST",
            PatternName = "最新ログテスト",
            SourceTableName = "テスト",
            IsActive = true,
            Details = new List<AutoJournalPatternDetail>
            {
                new AutoJournalPatternDetail
                {
                    AutoJournalPatternId = 0,
                    LineNumber = 1,
                    DebitCreditFlag = "D",
                    AccountCode = "1100",
                    AmountExpression = "金額"
                }
            }
        });

        // 古いログ
        await _repository.InsertExecutionLogAsync(new AutoJournalExecutionLog
        {
            AutoJournalPatternId = patternId,
            ExecutedAt = new DateTime(2025, 1, 20, 10, 0, 0),
            ProcessedCount = 50,
            GeneratedCount = 50,
            Status = "SUCCESS"
        });

        // 新しいログ
        await _repository.InsertExecutionLogAsync(new AutoJournalExecutionLog
        {
            AutoJournalPatternId = patternId,
            ExecutedAt = new DateTime(2025, 1, 21, 15, 0, 0),
            ProcessedCount = 100,
            GeneratedCount = 100,
            Status = "SUCCESS",
            Message = "最新の処理"
        });

        var latest = await _repository.FindLatestExecutionLogAsync(patternId);
        latest.Should().NotBeNull();
        latest!.ProcessedCount.Should().Be(100);
        latest.Message.Should().Be("最新の処理");
    }

    #endregion
}
