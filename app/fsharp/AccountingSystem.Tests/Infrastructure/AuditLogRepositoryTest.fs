module AccountingSystem.Tests.Infrastructure.AuditLogRepositoryTest

open System
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models
open AccountingSystem.Infrastructure.Persistence.Repositories
open AccountingSystem.Tests.DatabaseTestBase
open Npgsql
open Xunit
open FsUnit.Xunit

/// <summary>
/// 監査ログリポジトリ - Dapper 統合テスト
/// </summary>
type AuditLogRepositoryTest() =
    inherit DatabaseTestBase()

    /// テスト用の監査ログを作成
    member private _.CreateTestAuditLog
        (entityType: string)
        (entityId: string)
        (action: AuditAction)
        (userId: string)
        (userName: string)
        (timestamp: DateTime) : AuditLog =
        {
            Id = None
            EntityType = entityType
            EntityId = entityId
            Action = action
            UserId = userId
            UserName = userName
            Timestamp = timestamp
            OldValues = Some "{\"name\": \"old\"}"
            NewValues = Some "{\"name\": \"new\"}"
            Changes = Some "{\"name\": [\"old\", \"new\"]}"
            Reason = Some "テスト理由"
            IpAddress = Some "192.168.1.1"
            UserAgent = Some "TestAgent/1.0"
        }

    /// テストデータをクリーンアップ
    member private this.CleanupTestDataAsync() =
        task {
            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()

            use cmd = new NpgsqlCommand("""
                DELETE FROM audit_log WHERE entity_id LIKE 'TEST%';
            """, conn)
            let! _ = cmd.ExecuteNonQueryAsync()
            ()
        }

    [<Fact>]
    member this.``監査ログを登録できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let auditLog = this.CreateTestAuditLog
                            "Account" "TEST001" AuditAction.Create
                            "user1" "テストユーザー1" DateTime.UtcNow

            let! inserted = repository.InsertAsync(auditLog)

            inserted.Id.IsSome |> should equal true
            inserted.EntityType |> should equal "Account"
            inserted.EntityId |> should equal "TEST001"
            inserted.Action |> should equal AuditAction.Create

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``エンティティ別の監査ログを取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            // 同じエンティティに対して複数のログを登録
            let log1 = this.CreateTestAuditLog
                        "Account" "TEST002" AuditAction.Create
                        "user1" "ユーザー1" (DateTime(2024, 1, 15, 10, 0, 0))
            let log2 = this.CreateTestAuditLog
                        "Account" "TEST002" AuditAction.Update
                        "user1" "ユーザー1" (DateTime(2024, 1, 16, 10, 0, 0))
            let log3 = this.CreateTestAuditLog
                        "Journal" "TEST003" AuditAction.Create
                        "user2" "ユーザー2" (DateTime(2024, 1, 17, 10, 0, 0))

            let! _ = repository.InsertAsync(log1)
            let! _ = repository.InsertAsync(log2)
            let! _ = repository.InsertAsync(log3)

            let! logs = repository.FindByEntityAsync("Account", "TEST002")

            logs.Length |> should equal 2
            logs |> List.forall (fun l -> l.EntityType = "Account" && l.EntityId = "TEST002")
                |> should equal true

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``ユーザー別の監査ログを期間指定で取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let log1 = this.CreateTestAuditLog
                        "Account" "TEST004" AuditAction.Create
                        "testuser1" "テストユーザー1" (DateTime(2024, 1, 10, 10, 0, 0))
            let log2 = this.CreateTestAuditLog
                        "Account" "TEST005" AuditAction.Update
                        "testuser1" "テストユーザー1" (DateTime(2024, 1, 20, 10, 0, 0))
            let log3 = this.CreateTestAuditLog
                        "Journal" "TEST006" AuditAction.Create
                        "testuser2" "テストユーザー2" (DateTime(2024, 1, 15, 10, 0, 0))

            let! _ = repository.InsertAsync(log1)
            let! _ = repository.InsertAsync(log2)
            let! _ = repository.InsertAsync(log3)

            let! logs = repository.FindByUserAsync(
                            "testuser1",
                            DateTime(2024, 1, 1),
                            DateTime(2024, 1, 31))

            logs.Length |> should equal 2
            logs |> List.forall (fun l -> l.UserId = "testuser1")
                |> should equal true

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``期間内のすべての監査ログを取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let log1 = this.CreateTestAuditLog
                        "Account" "TEST007" AuditAction.Create
                        "user1" "ユーザー1" (DateTime(2024, 1, 10, 10, 0, 0))
            let log2 = this.CreateTestAuditLog
                        "Account" "TEST008" AuditAction.Update
                        "user1" "ユーザー1" (DateTime(2024, 1, 15, 10, 0, 0))
            let log3 = this.CreateTestAuditLog
                        "Journal" "TEST009" AuditAction.Create
                        "user2" "ユーザー2" (DateTime(2024, 1, 25, 10, 0, 0))

            let! _ = repository.InsertAsync(log1)
            let! _ = repository.InsertAsync(log2)
            let! _ = repository.InsertAsync(log3)

            let! logs = repository.FindByDateRangeAsync(
                            DateTime(2024, 1, 12),
                            DateTime(2024, 1, 20))

            logs.Length |> should equal 1
            logs.Head.EntityId |> should equal "TEST008"

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``アクション種別で監査ログを検索できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let log1 = this.CreateTestAuditLog
                        "Account" "TEST010" AuditAction.Create
                        "user1" "ユーザー1" (DateTime(2024, 1, 15, 10, 0, 0))
            let log2 = this.CreateTestAuditLog
                        "Account" "TEST011" AuditAction.Update
                        "user1" "ユーザー1" (DateTime(2024, 1, 16, 10, 0, 0))
            let log3 = this.CreateTestAuditLog
                        "Journal" "TEST012" AuditAction.Delete
                        "user2" "ユーザー2" (DateTime(2024, 1, 17, 10, 0, 0))

            let! _ = repository.InsertAsync(log1)
            let! _ = repository.InsertAsync(log2)
            let! _ = repository.InsertAsync(log3)

            let! createLogs = repository.FindByActionAsync(
                                "CREATE",
                                DateTime(2024, 1, 1),
                                DateTime(2024, 1, 31))

            createLogs.Length |> should equal 1
            createLogs.Head.Action |> should equal AuditAction.Create

            let! deleteLogs = repository.FindByActionAsync(
                                "DELETE",
                                DateTime(2024, 1, 1),
                                DateTime(2024, 1, 31))

            deleteLogs.Length |> should equal 1
            deleteLogs.Head.Action |> should equal AuditAction.Delete

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``存在しないエンティティの監査ログは空リストを返す``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let! logs = repository.FindByEntityAsync("Account", "NONEXISTENT")

            logs.Length |> should equal 0
        }

    [<Fact>]
    member this.``JSONB形式のデータが正しく保存・取得できる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let auditLog = {
                Id = None
                EntityType = "Account"
                EntityId = "TEST013"
                Action = AuditAction.Update
                UserId = "user1"
                UserName = "テストユーザー"
                Timestamp = DateTime.UtcNow
                OldValues = Some "{\"balance\": 10000, \"name\": \"旧名称\"}"
                NewValues = Some "{\"balance\": 20000, \"name\": \"新名称\"}"
                Changes = Some "{\"balance\": [10000, 20000], \"name\": [\"旧名称\", \"新名称\"]}"
                Reason = Some "残高修正"
                IpAddress = Some "10.0.0.1"
                UserAgent = Some "Mozilla/5.0"
            }

            let! inserted = repository.InsertAsync(auditLog)
            let! logs = repository.FindByEntityAsync("Account", "TEST013")

            logs.Length |> should equal 1
            let retrieved = logs.Head

            // JSONB は順序を保証しないため、値が存在することのみ確認
            retrieved.OldValues.IsSome |> should equal true
            retrieved.OldValues.Value.Contains("balance") |> should equal true
            retrieved.OldValues.Value.Contains("10000") |> should equal true
            retrieved.NewValues.IsSome |> should equal true
            retrieved.NewValues.Value.Contains("20000") |> should equal true
            retrieved.Changes.IsSome |> should equal true
            retrieved.Reason |> should equal (Some "残高修正")

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``NULLのオプション値が正しく処理される``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let auditLog = {
                Id = None
                EntityType = "Account"
                EntityId = "TEST014"
                Action = AuditAction.Create
                UserId = "user1"
                UserName = "テストユーザー"
                Timestamp = DateTime.UtcNow
                OldValues = None
                NewValues = Some "{\"name\": \"新規作成\"}"
                Changes = None
                Reason = None
                IpAddress = None
                UserAgent = None
            }

            let! _ = repository.InsertAsync(auditLog)
            let! logs = repository.FindByEntityAsync("Account", "TEST014")

            logs.Length |> should equal 1
            let retrieved = logs.Head

            retrieved.OldValues |> should equal None
            retrieved.Changes |> should equal None
            retrieved.Reason |> should equal None
            retrieved.IpAddress |> should equal None
            retrieved.UserAgent |> should equal None

            do! this.CleanupTestDataAsync()
        }

    [<Fact>]
    member this.``監査ログは時系列順（降順）でソートされる``() =
        task {
            do! this.CleanupTestDataAsync()

            use conn = new NpgsqlConnection(this.ConnectionString)
            do! conn.OpenAsync()
            let repository = AuditLogRepository(conn)

            let log1 = this.CreateTestAuditLog
                        "Account" "TEST015" AuditAction.Create
                        "user1" "ユーザー1" (DateTime(2024, 1, 10, 10, 0, 0))
            let log2 = this.CreateTestAuditLog
                        "Account" "TEST015" AuditAction.Update
                        "user1" "ユーザー1" (DateTime(2024, 1, 20, 10, 0, 0))
            let log3 = this.CreateTestAuditLog
                        "Account" "TEST015" AuditAction.Update
                        "user2" "ユーザー2" (DateTime(2024, 1, 15, 10, 0, 0))

            let! _ = repository.InsertAsync(log1)
            let! _ = repository.InsertAsync(log2)
            let! _ = repository.InsertAsync(log3)

            let! logs = repository.FindByEntityAsync("Account", "TEST015")

            logs.Length |> should equal 3
            // 降順でソートされていることを確認
            logs.[0].Timestamp |> should be (greaterThan logs.[1].Timestamp)
            logs.[1].Timestamp |> should be (greaterThan logs.[2].Timestamp)

            do! this.CleanupTestDataAsync()
        }
