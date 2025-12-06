namespace AccountingSystem.Infrastructure.Seed

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Npgsql
open Dapper
open AccountingSystem.Infrastructure.Seed.AccountingSeedData

/// <summary>
/// データベース Seeder サービス
/// IHostedService として実装し、アプリケーション起動時に Seed データを投入
/// </summary>
type DatabaseSeeder(connectionString: string, logger: ILogger<DatabaseSeeder>) =

    /// <summary>
    /// テーブルが存在するかチェック
    /// </summary>
    let tableExists (connection: NpgsqlConnection) (tableName: string) =
        task {
            let sql = "SELECT EXISTS (SELECT FROM information_schema.tables WHERE table_schema = 'public' AND table_name = @TableName)"
            let! exists = connection.ExecuteScalarAsync<bool>(sql, {| TableName = tableName |})
            return exists
        }

    /// <summary>
    /// Seed データが既に存在するかチェック
    /// </summary>
    let isSeedDataExists (connection: NpgsqlConnection) =
        task {
            // テーブルが存在しない場合は false を返す
            let! accountMasterExists = tableExists connection "勘定科目マスタ"
            if not accountMasterExists then
                return false
            else
                let sql = """SELECT COUNT(*) FROM "勘定科目マスタ" WHERE "勘定科目コード" = '1'"""
                let! count = connection.ExecuteScalarAsync<int>(sql)
                return count > 0
        }

    /// <summary>
    /// 必要なテーブルがすべて存在するかチェック
    /// </summary>
    let allTablesExist (connection: NpgsqlConnection) =
        task {
            let! accountMaster = tableExists connection "勘定科目マスタ"
            let! accountStructure = tableExists connection "勘定科目構成マスタ"
            let! dailyBalance = tableExists connection "日次勘定科目残高"
            return accountMaster && accountStructure && dailyBalance
        }

    /// <summary>
    /// 勘定科目マスタを Seed
    /// </summary>
    let seedAccounts (connection: NpgsqlConnection) =
        task {
            let accounts = getAccounts ()
            let sql = """
                INSERT INTO "勘定科目マスタ" (
                    "勘定科目コード", "勘定科目名", "勘定科目カナ", "勘定科目種別",
                    "合計科目", "BSPL区分", "取引要素区分",
                    "費用区分", "表示順序"
                ) VALUES (
                    @AccountCode, @AccountName, @AccountKana, @AccountType::account_type,
                    @IsSummaryAccount, @BsplType, @TransactionElement,
                    @ExpenseCategory, @DisplayOrder
                )
                ON CONFLICT ("勘定科目コード") DO NOTHING
            """

            for account in accounts do
                let parameters = {|
                    AccountCode = account.AccountCode
                    AccountName = account.AccountName
                    AccountKana = account.AccountKana
                    AccountType = account.AccountType
                    IsSummaryAccount = account.IsSummaryAccount
                    BsplType = account.BsplType
                    TransactionElement = account.TransactionElement
                    ExpenseCategory = account.ExpenseCategory
                    DisplayOrder = account.DisplayOrder
                |}
                let! _ = connection.ExecuteAsync(sql, parameters)
                ()

            logger.LogInformation("Seeded {Count} accounts", accounts.Length)
        }

    /// <summary>
    /// 勘定科目構成マスタを Seed
    /// </summary>
    let seedAccountStructures (connection: NpgsqlConnection) =
        task {
            let structures = getAccountStructures ()
            let sql = """
                INSERT INTO "勘定科目構成マスタ" ("勘定科目コード", "勘定科目パス")
                VALUES (@AccountCode, @AccountPath)
                ON CONFLICT ("勘定科目コード") DO NOTHING
            """

            for structure in structures do
                let parameters = {|
                    AccountCode = structure.AccountCode
                    AccountPath = structure.AccountPath
                |}
                let! _ = connection.ExecuteAsync(sql, parameters)
                ()

            logger.LogInformation("Seeded {Count} account structures", structures.Length)
        }

    /// <summary>
    /// 日次勘定科目残高を Seed
    /// </summary>
    let seedDailyBalances (connection: NpgsqlConnection) =
        task {
            let fy2021Balances = getFY2021DailyBalances ()
            let fy2022Balances = getFY2022DailyBalances ()
            let allBalances = fy2021Balances @ fy2022Balances

            let sql = """
                INSERT INTO "日次勘定科目残高" (
                    "勘定科目コード", "起票日", "借方金額", "貸方金額"
                ) VALUES (
                    @AccountCode, @PostingDate, @DebitAmount, @CreditAmount
                )
                ON CONFLICT ("勘定科目コード", "起票日", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ") DO NOTHING
            """

            for balance in allBalances do
                let parameters = {|
                    AccountCode = balance.AccountCode
                    PostingDate = balance.PostingDate
                    DebitAmount = balance.DebitAmount
                    CreditAmount = balance.CreditAmount
                |}
                let! _ = connection.ExecuteAsync(sql, parameters)
                ()

            logger.LogInformation("Seeded {Count} daily balances", allBalances.Length)
        }

    /// <summary>
    /// 全ての Seed データを投入
    /// </summary>
    member _.SeedAsync() =
        task {
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            // テーブルが存在しない場合はスキップ（マイグレーション未実行）
            let! tablesExist = allTablesExist connection
            if not tablesExist then
                logger.LogWarning("Required tables do not exist. Skipping seeding (migration not yet run).")
                return ()

            // 既にデータが存在する場合はスキップ
            let! exists = isSeedDataExists connection
            if exists then
                logger.LogInformation("Seed data already exists. Skipping seeding.")
                return ()

            logger.LogInformation("Starting database seeding...")

            // トランザクション内で Seed を実行
            use! transaction = connection.BeginTransactionAsync()
            try
                do! seedAccounts connection
                do! seedAccountStructures connection
                do! seedDailyBalances connection

                do! transaction.CommitAsync()
                logger.LogInformation("Database seeding completed successfully.")
            with ex ->
                do! transaction.RollbackAsync()
                logger.LogError(ex, "Database seeding failed. Rolled back.")
                raise ex
        }

    /// <summary>
    /// 強制的に Seed データを再投入（テスト用）
    /// </summary>
    member _.ForceSeedAsync() =
        task {
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            // テーブルが存在しない場合はスキップ
            let! tablesExist = allTablesExist connection
            if not tablesExist then
                logger.LogWarning("Required tables do not exist. Skipping force seeding.")
                return ()

            logger.LogInformation("Force seeding database (clearing existing data)...")

            // 既存データをクリア
            let clearSql = """
                DELETE FROM "日次勘定科目残高" WHERE "勘定科目コード" ~ '^[0-9]+$';
                DELETE FROM "勘定科目構成マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$';
                DELETE FROM "勘定科目マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$';
            """
            let! _ = connection.ExecuteAsync(clearSql)

            // 再 Seed
            use! transaction = connection.BeginTransactionAsync()
            try
                do! seedAccounts connection
                do! seedAccountStructures connection
                do! seedDailyBalances connection

                do! transaction.CommitAsync()
                logger.LogInformation("Force database seeding completed successfully.")
            with ex ->
                do! transaction.RollbackAsync()
                logger.LogError(ex, "Force database seeding failed. Rolled back.")
                raise ex
        }

    interface IHostedService with
        member this.StartAsync(cancellationToken: CancellationToken) =
            task {
                try
                    do! this.SeedAsync()
                with ex ->
                    logger.LogWarning(ex, "Database seeding skipped or failed during startup.")
            } :> Task

        member _.StopAsync(_cancellationToken: CancellationToken) =
            Task.CompletedTask
