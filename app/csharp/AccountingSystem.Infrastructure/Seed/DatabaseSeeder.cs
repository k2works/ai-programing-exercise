using AccountingSystem.Infrastructure.Persistence.Repositories;
using Dapper;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Npgsql;

namespace AccountingSystem.Infrastructure.Seed;

/// <summary>
/// データベース Seed サービス
/// アプリケーション起動時に D 社の財務データを投入する IHostedService
/// </summary>
public class DatabaseSeeder : IHostedService
{
    private readonly string _connectionString;
    private readonly ILogger<DatabaseSeeder> _logger;
    private readonly bool _seedOnStartup;

    static DatabaseSeeder()
    {
        // DateOnly 型の Dapper TypeHandler を登録
        SqlMapper.AddTypeHandler(new DateOnlyTypeHandler());
    }

    public DatabaseSeeder(
        IConfiguration configuration,
        ILogger<DatabaseSeeder> logger)
    {
        _connectionString = configuration.GetConnectionString("DefaultConnection")
            ?? throw new InvalidOperationException("接続文字列 'DefaultConnection' が設定されていません。");
        _logger = logger;
        _seedOnStartup = configuration.GetValue<bool>("Seed:EnableOnStartup", false);
    }

    public async Task StartAsync(CancellationToken cancellationToken)
    {
        if (!_seedOnStartup)
        {
            _logger.LogInformation("Seed:EnableOnStartup が false のため、Seed データの投入をスキップします。");
            return;
        }

        _logger.LogInformation("データベース Seed 処理を開始します...");

        try
        {
            await SeedDatabaseAsync(cancellationToken);
            _logger.LogInformation("データベース Seed 処理が完了しました。");
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "データベース Seed 処理でエラーが発生しました。");
            throw;
        }
    }

    public Task StopAsync(CancellationToken cancellationToken) => Task.CompletedTask;

    /// <summary>
    /// データベースに Seed データを投入（冪等性を保証）
    /// </summary>
    public async Task SeedDatabaseAsync(CancellationToken cancellationToken = default)
    {
        await using var connection = new NpgsqlConnection(_connectionString);
        await connection.OpenAsync(cancellationToken);

        // 既存データがあるかチェック
        var existingCount = await connection.ExecuteScalarAsync<int>(
            @"SELECT COUNT(*) FROM ""勘定科目マスタ""");

        if (existingCount > 0)
        {
            _logger.LogInformation("勘定科目マスタに既存データが {Count} 件あります。Seed をスキップします。", existingCount);
            return;
        }

        await using var transaction = await connection.BeginTransactionAsync(cancellationToken);

        try
        {
            // 1. 勘定科目マスタを投入
            await SeedAccountsAsync(connection, transaction);

            // 2. 勘定科目構成マスタを投入
            await SeedAccountStructuresAsync(connection, transaction);

            // 3. 日次残高データを投入（FY2021, FY2022）
            await SeedDailyBalancesAsync(connection, transaction);

            // 4. 月次残高データを投入（FY2021, FY2022）
            await SeedMonthlyBalancesAsync(connection, transaction);

            await transaction.CommitAsync(cancellationToken);
            _logger.LogInformation("Seed データの投入が完了しました。");
        }
        catch
        {
            await transaction.RollbackAsync(cancellationToken);
            throw;
        }
    }

    private async Task SeedAccountsAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        var accounts = AccountingSeedData.GetAccounts();
        _logger.LogInformation("勘定科目マスタを {Count} 件投入します...", accounts.Count);

        var sql = @"
            INSERT INTO ""勘定科目マスタ"" (
                ""勘定科目コード"",
                ""勘定科目名"",
                ""勘定科目カナ"",
                ""勘定科目種別"",
                ""合計科目"",
                ""BSPL区分"",
                ""取引要素区分"",
                ""費用区分"",
                ""表示順序"",
                ""集計対象"",
                ""課税取引コード"",
                ""残高"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @AccountCode,
                @AccountName,
                @AccountNameKana,
                @AccountType::account_type,
                @IsSummaryAccount,
                @BsplType,
                @TransactionElementType,
                @ExpenseType,
                @DisplayOrder,
                @IsAggregationTarget,
                @TaxCode,
                @Balance,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        ";

        var displayOrder = 1;
        foreach (var accountData in accounts)
        {
            var account = new
            {
                AccountCode = accountData.AccountCode,
                AccountName = accountData.AccountName,
                AccountNameKana = (string?)null,
                AccountType = accountData.AccountType,
                IsSummaryAccount = accountData.Level < 3, // Level 1-2 は合計科目
                BsplType = GetBsplType(accountData.AccountType),
                TransactionElementType = GetTransactionElementType(accountData.AccountType),
                ExpenseType = (string?)null,
                DisplayOrder = displayOrder++,
                IsAggregationTarget = true,
                TaxCode = (string?)null,
                Balance = 0m
            };

            await connection.ExecuteAsync(sql, account, transaction);
        }

        _logger.LogInformation("勘定科目マスタの投入が完了しました。");
    }

    private async Task SeedAccountStructuresAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        var structures = AccountingSeedData.GetAccountStructures();
        _logger.LogInformation("勘定科目構成マスタを {Count} 件投入します...", structures.Count);

        var sql = @"
            INSERT INTO ""勘定科目構成マスタ"" (
                ""勘定科目コード"",
                ""勘定科目パス"",
                ""階層レベル"",
                ""親科目コード"",
                ""表示順序"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @AccountCode,
                @AccountPath,
                @HierarchyLevel,
                @ParentAccountCode,
                @DisplayOrder,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        ";

        var displayOrder = 1;
        foreach (var structureData in structures)
        {
            var pathParts = structureData.AccountPath.Split('~');
            var hierarchyLevel = pathParts.Length;
            var parentAccountCode = hierarchyLevel > 1 ? pathParts[^2] : null;

            var structure = new
            {
                AccountCode = structureData.AccountCode,
                AccountPath = structureData.AccountPath,
                HierarchyLevel = hierarchyLevel,
                ParentAccountCode = parentAccountCode,
                DisplayOrder = displayOrder++
            };

            await connection.ExecuteAsync(sql, structure, transaction);
        }

        _logger.LogInformation("勘定科目構成マスタの投入が完了しました。");
    }

    private async Task SeedDailyBalancesAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        var fy2021Balances = AccountingSeedData.GetFY2021DailyBalances();
        var fy2022Balances = AccountingSeedData.GetFY2022DailyBalances();
        var allBalances = fy2021Balances.Concat(fy2022Balances).ToList();

        _logger.LogInformation("日次勘定科目残高を {Count} 件投入します...", allBalances.Count);

        var sql = @"
            INSERT INTO ""日次勘定科目残高"" (
                ""起票日"",
                ""勘定科目コード"",
                ""補助科目コード"",
                ""部門コード"",
                ""プロジェクトコード"",
                ""決算仕訳フラグ"",
                ""借方金額"",
                ""貸方金額"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @EntryDate,
                @AccountCode,
                @SubAccountCode,
                @DepartmentCode,
                @ProjectCode,
                @SettlementFlag,
                @DebitAmount,
                @CreditAmount,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        ";

        foreach (var balanceData in allBalances)
        {
            var balance = new
            {
                EntryDate = balanceData.BalanceDate,
                AccountCode = balanceData.AccountCode,
                SubAccountCode = "",
                DepartmentCode = "",
                ProjectCode = "",
                SettlementFlag = 0,
                DebitAmount = balanceData.DebitAmount,
                CreditAmount = balanceData.CreditAmount
            };

            await connection.ExecuteAsync(sql, balance, transaction);
        }

        _logger.LogInformation("日次勘定科目残高の投入が完了しました。");
    }

    private async Task SeedMonthlyBalancesAsync(NpgsqlConnection connection, NpgsqlTransaction transaction)
    {
        var fy2021Balances = AccountingSeedData.GetFY2021MonthlyBalances();
        var fy2022Balances = AccountingSeedData.GetFY2022MonthlyBalances();
        var allBalances = fy2021Balances.Concat(fy2022Balances).ToList();

        _logger.LogInformation("月次勘定科目残高を {Count} 件投入します...", allBalances.Count);

        var sql = @"
            INSERT INTO ""月次勘定科目残高"" (
                ""決算期"",
                ""月度"",
                ""勘定科目コード"",
                ""補助科目コード"",
                ""部門コード"",
                ""プロジェクトコード"",
                ""決算仕訳フラグ"",
                ""月初残高"",
                ""借方金額"",
                ""貸方金額"",
                ""月末残高"",
                ""作成日時"",
                ""更新日時""
            ) VALUES (
                @FiscalYear,
                @Month,
                @AccountCode,
                @SubAccountCode,
                @DepartmentCode,
                @ProjectCode,
                @SettlementFlag,
                @OpeningBalance,
                @DebitAmount,
                @CreditAmount,
                @ClosingBalance,
                CURRENT_TIMESTAMP,
                CURRENT_TIMESTAMP
            )
        ";

        foreach (var balanceData in allBalances)
        {
            var balance = new
            {
                FiscalYear = balanceData.FiscalYear,
                Month = balanceData.Month,
                AccountCode = balanceData.AccountCode,
                SubAccountCode = "",
                DepartmentCode = "",
                ProjectCode = "",
                SettlementFlag = 0,
                OpeningBalance = balanceData.OpeningBalance,
                DebitAmount = balanceData.DebitAmount,
                CreditAmount = balanceData.CreditAmount,
                ClosingBalance = balanceData.ClosingBalance
            };

            await connection.ExecuteAsync(sql, balance, transaction);
        }

        _logger.LogInformation("月次勘定科目残高の投入が完了しました。");
    }

    /// <summary>
    /// 勘定科目種別から BSPL 区分を取得
    /// B: 貸借対照表, P: 損益計算書
    /// </summary>
    private static string GetBsplType(string accountType)
    {
        return accountType switch
        {
            "資産" => "B",
            "負債" => "B",
            "純資産" => "B",
            "収益" => "P",
            "費用" => "P",
            _ => "B"
        };
    }

    /// <summary>
    /// 勘定科目種別から取引要素区分を取得
    /// 1: 資産, 2: 負債, 3: 純資産, 4: 収益, 5: 費用
    /// </summary>
    private static string GetTransactionElementType(string accountType)
    {
        return accountType switch
        {
            "資産" => "1",
            "負債" => "2",
            "純資産" => "3",
            "収益" => "4",
            "費用" => "5",
            _ => "1"
        };
    }
}
