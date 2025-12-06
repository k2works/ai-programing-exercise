module AccountingSystem.Tests.Integration.DatabaseSeederIntegrationTest

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open Npgsql
open Dapper
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
open AccountingSystem.Tests.DatabaseTestBase
open AccountingSystem.Infrastructure.Seed
open AccountingSystem.Infrastructure.Seed.AccountingSeedData

/// <summary>
/// DatabaseSeeder 統合テスト
/// Testcontainers を使用して PostgreSQL コンテナで実際に Seed を実行
/// </summary>
type DatabaseSeederIntegrationTest() =
    inherit DatabaseTestBase()

    let logger = NullLogger<DatabaseSeeder>.Instance

    /// <summary>
    /// テストデータのクリーンアップ
    /// テーブルが存在しない場合はスキップ
    /// </summary>
    member this.CleanupTestDataAsync() =
        task {
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // テーブルが存在する場合のみ削除を実行
            let sql = """
                DO $$
                BEGIN
                    IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '日次勘定科目残高') THEN
                        DELETE FROM "日次勘定科目残高" WHERE "勘定科目コード" ~ '^[0-9]+$';
                    END IF;
                    IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '勘定科目構成マスタ') THEN
                        DELETE FROM "勘定科目構成マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$';
                    END IF;
                    IF EXISTS (SELECT FROM information_schema.tables WHERE table_name = '勘定科目マスタ') THEN
                        DELETE FROM "勘定科目マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$';
                    END IF;
                END $$;
            """

            let! _ = connection.ExecuteAsync(sql)
            return ()
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``SeedAsync で勘定科目マスタが投入される``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let! count = connection.ExecuteScalarAsync<int>("""SELECT COUNT(*) FROM "勘定科目マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$'""")
            let expectedCount = getAccounts().Length

            count |> should equal expectedCount
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``SeedAsync で勘定科目構成マスタが投入される``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let! count = connection.ExecuteScalarAsync<int>("""SELECT COUNT(*) FROM "勘定科目構成マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$'""")
            let expectedCount = getAccountStructures().Length

            count |> should equal expectedCount
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``SeedAsync で日次残高データが投入される``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            let! count = connection.ExecuteScalarAsync<int>("""SELECT COUNT(*) FROM "日次勘定科目残高" WHERE "勘定科目コード" ~ '^[0-9]+$'""")
            let expectedCount = getFY2021DailyBalances().Length + getFY2022DailyBalances().Length

            count |> should equal expectedCount
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``SeedAsync は既にデータがある場合スキップされる``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // 1回目の Seed
            do! seeder.SeedAsync()

            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()
            let! countBefore = connection.ExecuteScalarAsync<int>("""SELECT COUNT(*) FROM "勘定科目マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$'""")

            // Act - 2回目の Seed（スキップされるはず）
            do! seeder.SeedAsync()

            // Assert
            let! countAfter = connection.ExecuteScalarAsync<int>("""SELECT COUNT(*) FROM "勘定科目マスタ" WHERE "勘定科目コード" ~ '^[0-9]+$'""")
            countAfter |> should equal countBefore
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``ForceSeedAsync は既存データを削除して再投入する``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // 1回目の Seed
            do! seeder.SeedAsync()

            // データを変更
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()
            let! _ = connection.ExecuteAsync("""UPDATE "勘定科目マスタ" SET "勘定科目名" = 'MODIFIED' WHERE "勘定科目コード" = '1'""")

            // Act - 強制 Seed
            do! seeder.ForceSeedAsync()

            // Assert
            let! name = connection.ExecuteScalarAsync<string>("""SELECT "勘定科目名" FROM "勘定科目マスタ" WHERE "勘定科目コード" = '1'""")
            name |> should equal "資産"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``Seed された勘定科目構成の階層パスが正しい``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 流動資産(11)のパスが 1~11 であることを確認
            let! path11 = connection.ExecuteScalarAsync<string>("""SELECT "勘定科目パス" FROM "勘定科目構成マスタ" WHERE "勘定科目コード" = '11'""")
            path11 |> should equal "1~11"

            // 現金預金(111)のパスが 1~11~111 であることを確認
            let! path111 = connection.ExecuteScalarAsync<string>("""SELECT "勘定科目パス" FROM "勘定科目構成マスタ" WHERE "勘定科目コード" = '111'""")
            path111 |> should equal "1~11~111"

            // 建物及び構築物(1211)のパスが 1~12~121~1211 であることを確認
            let! path1211 = connection.ExecuteScalarAsync<string>("""SELECT "勘定科目パス" FROM "勘定科目構成マスタ" WHERE "勘定科目コード" = '1211'""")
            path1211 |> should equal "1~12~121~1211"
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``Seed された日次残高の金額が正しい（令和3年度期末）``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 令和3年度期末（2022/3/31）の現金預金残高
            let! cashBalance = connection.ExecuteScalarAsync<decimal>(
                """SELECT "借方金額" FROM "日次勘定科目残高" WHERE "勘定科目コード" = '111' AND "起票日" = '2022-03-31'"""
            )
            cashBalance |> should equal 593256m

            // 令和3年度期末の売上高
            let! salesBalance = connection.ExecuteScalarAsync<decimal>(
                """SELECT "貸方金額" FROM "日次勘定科目残高" WHERE "勘定科目コード" = '41' AND "起票日" = '2022-03-31'"""
            )
            salesBalance |> should equal 5796105m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``Seed された日次残高の金額が正しい（令和4年度期末）``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 令和4年度期末（2023/3/31）の現金預金残高
            let! cashBalance = connection.ExecuteScalarAsync<decimal>(
                """SELECT "借方金額" FROM "日次勘定科目残高" WHERE "勘定科目コード" = '111' AND "起票日" = '2023-03-31'"""
            )
            cashBalance |> should equal 1133270m

            // 令和4年度期末の売上高
            let! salesBalance = connection.ExecuteScalarAsync<decimal>(
                """SELECT "貸方金額" FROM "日次勘定科目残高" WHERE "勘定科目コード" = '41' AND "起票日" = '2023-03-31'"""
            )
            salesBalance |> should equal 4547908m
        }

    [<Fact>]
    [<Trait("Category", "Integration")>]
    member this.``Seed された勘定科目の属性が正しい``() : Task =
        task {
            // Arrange
            do! this.CleanupTestDataAsync()
            let seeder = DatabaseSeeder(this.ConnectionString, logger)

            // Act
            do! seeder.SeedAsync()

            // Assert
            use connection = new NpgsqlConnection(this.ConnectionString)
            do! connection.OpenAsync()

            // 資産(1)は集計勘定
            let! isSummary1 = connection.ExecuteScalarAsync<bool>(
                """SELECT "合計科目" FROM "勘定科目マスタ" WHERE "勘定科目コード" = '1'"""
            )
            isSummary1 |> should equal true

            // 現金預金(111)は集計勘定ではない
            let! isSummary111 = connection.ExecuteScalarAsync<bool>(
                """SELECT "合計科目" FROM "勘定科目マスタ" WHERE "勘定科目コード" = '111'"""
            )
            isSummary111 |> should equal false

            // 売上原価(51)の費用区分は '1'
            let! expenseCategory = connection.ExecuteScalarAsync<string>(
                """SELECT "費用区分" FROM "勘定科目マスタ" WHERE "勘定科目コード" = '51'"""
            )
            expenseCategory |> should equal "1"
        }
