module AccountingSystem.Tests.Repositories.AccountStructureRepositoryTest

open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Types
open AccountingSystem.Infrastructure.Repositories.AccountStructureRepository
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit

/// <summary>
/// 勘定科目構成マスタ - Dapper 統合テスト
/// </summary>
type AccountStructureRepositoryTest() =
    inherit DatabaseTestBase()

    /// テスト用の勘定科目マスタデータを登録
    member private this.SetupTestAccountsAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            let sql = """
                INSERT INTO "勘定科目マスタ" (
                    "勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高"
                ) VALUES
                    ('11', '資産の部', '資産'::account_type, true, true, 0),
                    ('11000', '流動資産', '資産'::account_type, true, true, 0),
                    ('11190', '現金及び預金', '資産'::account_type, true, true, 0),
                    ('11110', '現金', '資産'::account_type, false, true, 100000),
                    ('11120', '当座預金', '資産'::account_type, false, true, 500000),
                    ('11130', '普通預金', '資産'::account_type, false, true, 1000000)
                ON CONFLICT ("勘定科目コード") DO NOTHING
            """

            use cmd = new NpgsqlCommand(sql, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    /// テストデータをクリーンアップ
    member private this.CleanupAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM "勘定科目構成マスタ";
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``勘定科目構成を登録できる``() =
        task {
            do! this.CleanupAsync()
            do! this.SetupTestAccountsAsync()

            // 階層構造を登録
            let root = {
                AccountCode = AccountCode.Create("11")
                AccountPath = "11"
                HierarchyLevel = 1
                ParentAccountCode = None
                DisplayOrder = 1
            }
            do! insertAsync this.ConnectionString root

            let level2 = {
                AccountCode = AccountCode.Create("11000")
                AccountPath = "11~11000"
                HierarchyLevel = 2
                ParentAccountCode = Some (AccountCode.Create("11"))
                DisplayOrder = 1
            }
            do! insertAsync this.ConnectionString level2

            // 登録を確認
            let! found = findByCodeAsync this.ConnectionString "11"
            found.IsSome |> should equal true
            found.Value.AccountPath |> should equal "11"
            found.Value.HierarchyLevel |> should equal 1

            let! found2 = findByCodeAsync this.ConnectionString "11000"
            found2.IsSome |> should equal true
            found2.Value.AccountPath |> should equal "11~11000"
            found2.Value.HierarchyLevel |> should equal 2
        }

    [<Fact>]
    member this.``子孫科目を検索できる``() =
        task {
            do! this.CleanupAsync()
            do! this.SetupTestAccountsAsync()

            // 階層構造を登録
            let structures = [
                { AccountCode = AccountCode.Create("11"); AccountPath = "11"; HierarchyLevel = 1; ParentAccountCode = None; DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11000"); AccountPath = "11~11000"; HierarchyLevel = 2; ParentAccountCode = Some (AccountCode.Create("11")); DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11190"); AccountPath = "11~11000~11190"; HierarchyLevel = 3; ParentAccountCode = Some (AccountCode.Create("11000")); DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11110"); AccountPath = "11~11000~11190~11110"; HierarchyLevel = 4; ParentAccountCode = Some (AccountCode.Create("11190")); DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11120"); AccountPath = "11~11000~11190~11120"; HierarchyLevel = 4; ParentAccountCode = Some (AccountCode.Create("11190")); DisplayOrder = 2 }
                { AccountCode = AccountCode.Create("11130"); AccountPath = "11~11000~11190~11130"; HierarchyLevel = 4; ParentAccountCode = Some (AccountCode.Create("11190")); DisplayOrder = 3 }
            ]

            for s in structures do
                do! insertAsync this.ConnectionString s

            // 「現金及び預金」(11190) 配下の子孫を検索
            let! descendants = findDescendantsAsync this.ConnectionString "11190"
            descendants |> List.length |> should equal 4  // 11190, 11110, 11120, 11130
            descendants |> List.map (fun s -> s.AccountCode.Code) |> should contain "11190"
            descendants |> List.map (fun s -> s.AccountCode.Code) |> should contain "11110"
            descendants |> List.map (fun s -> s.AccountCode.Code) |> should contain "11120"
            descendants |> List.map (fun s -> s.AccountCode.Code) |> should contain "11130"
        }

    [<Fact>]
    member this.``階層レベルで科目を検索できる``() =
        task {
            do! this.CleanupAsync()
            do! this.SetupTestAccountsAsync()

            // 階層構造を登録
            let structures = [
                { AccountCode = AccountCode.Create("11"); AccountPath = "11"; HierarchyLevel = 1; ParentAccountCode = None; DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11000"); AccountPath = "11~11000"; HierarchyLevel = 2; ParentAccountCode = Some (AccountCode.Create("11")); DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11190"); AccountPath = "11~11000~11190"; HierarchyLevel = 3; ParentAccountCode = Some (AccountCode.Create("11000")); DisplayOrder = 1 }
                { AccountCode = AccountCode.Create("11110"); AccountPath = "11~11000~11190~11110"; HierarchyLevel = 4; ParentAccountCode = Some (AccountCode.Create("11190")); DisplayOrder = 1 }
            ]

            for s in structures do
                do! insertAsync this.ConnectionString s

            // 階層レベル4の科目を検索
            let! level4 = findByLevelAsync this.ConnectionString 4
            level4 |> List.length |> should equal 1
            level4 |> List.head |> fun s -> s.AccountCode.Code |> should equal "11110"
        }

    [<Fact>]
    member this.``勘定科目構成を更新できる``() =
        task {
            do! this.CleanupAsync()
            do! this.SetupTestAccountsAsync()

            // 登録
            let structure = {
                AccountCode = AccountCode.Create("11")
                AccountPath = "11"
                HierarchyLevel = 1
                ParentAccountCode = None
                DisplayOrder = 1
            }
            do! insertAsync this.ConnectionString structure

            // 更新
            let updated = { structure with DisplayOrder = 99 }
            do! updateAsync this.ConnectionString updated

            // 確認
            let! found = findByCodeAsync this.ConnectionString "11"
            found.Value.DisplayOrder |> should equal 99
        }

    [<Fact>]
    member this.``勘定科目構成を削除できる``() =
        task {
            do! this.CleanupAsync()
            do! this.SetupTestAccountsAsync()

            // 登録
            let structure = {
                AccountCode = AccountCode.Create("11")
                AccountPath = "11"
                HierarchyLevel = 1
                ParentAccountCode = None
                DisplayOrder = 1
            }
            do! insertAsync this.ConnectionString structure

            // 削除
            do! deleteAsync this.ConnectionString "11"

            // 確認
            let! found = findByCodeAsync this.ConnectionString "11"
            found.IsNone |> should equal true
        }
