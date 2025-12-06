namespace AccountingSystem.Tests.Application

open System
open System.Threading.Tasks
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services

/// <summary>
/// モック監査ログリポジトリ
/// </summary>
type MockAuditLogRepository(auditLogs: AuditLog list ref) =
    let mutable nextId = 1L

    interface IAuditLogRepository with
        member _.InsertAsync(auditLog: AuditLog) =
            task {
                let newLog = { auditLog with Id = Some nextId }
                nextId <- nextId + 1L
                auditLogs := newLog :: !auditLogs
                return newLog
            }

        member _.FindByEntityAsync(entityType: string, entityId: string) =
            task {
                return
                    !auditLogs
                    |> List.filter (fun log ->
                        log.EntityType = entityType && log.EntityId = entityId)
                    |> List.sortByDescending (fun log -> log.Timestamp)
            }

        member _.FindByUserAsync(userId: string, startDate: DateTime, endDate: DateTime) =
            task {
                return
                    !auditLogs
                    |> List.filter (fun log ->
                        log.UserId = userId &&
                        log.Timestamp >= startDate &&
                        log.Timestamp <= endDate)
                    |> List.sortByDescending (fun log -> log.Timestamp)
            }

        member _.FindByDateRangeAsync(startDate: DateTime, endDate: DateTime) =
            task {
                return
                    !auditLogs
                    |> List.filter (fun log ->
                        log.Timestamp >= startDate &&
                        log.Timestamp <= endDate)
                    |> List.sortByDescending (fun log -> log.Timestamp)
            }

        member _.FindByActionAsync(action: string, startDate: DateTime, endDate: DateTime) =
            task {
                return
                    !auditLogs
                    |> List.filter (fun log ->
                        AuditAction.toCode log.Action = action &&
                        log.Timestamp >= startDate &&
                        log.Timestamp <= endDate)
                    |> List.sortByDescending (fun log -> log.Timestamp)
            }

/// <summary>
/// AuditLogService のユニットテスト
/// </summary>
type AuditLogServiceTest() =

    /// テスト用の監査ログを作成
    member _.CreateAuditLog
        (entityType: string)
        (entityId: string)
        (action: AuditAction)
        (userId: string)
        (userName: string) : AuditLog =
        {
            Id = None
            EntityType = entityType
            EntityId = entityId
            Action = action
            UserId = userId
            UserName = userName
            Timestamp = DateTime.UtcNow
            OldValues = None
            NewValues = Some "{\"name\": \"test\"}"
            Changes = Some "{\"name\": \"test\"}"
            Reason = None
            IpAddress = Some "192.168.1.1"
            UserAgent = None
        }

    /// テスト用の初期データを作成
    member this.CreateInitialAuditLogs() : AuditLog list =
        [
            { this.CreateAuditLog "Account" "1001" AuditAction.Create "user1" "ユーザー1"
                with Timestamp = DateTime(2024, 1, 15, 10, 0, 0); Id = Some 1L }
            { this.CreateAuditLog "Account" "1001" AuditAction.Update "user1" "ユーザー1"
                with Timestamp = DateTime(2024, 1, 16, 10, 0, 0); Id = Some 2L }
            { this.CreateAuditLog "Journal" "J-2024-0001" AuditAction.Create "user2" "ユーザー2"
                with Timestamp = DateTime(2024, 1, 17, 10, 0, 0); Id = Some 3L }
            { this.CreateAuditLog "Account" "1002" AuditAction.Delete "user1" "ユーザー1"
                with Timestamp = DateTime(2024, 1, 18, 10, 0, 0); Id = Some 4L }
        ]

    [<Fact>]
    member this.``監査ログを記録できる``() =
        task {
            // Given
            let auditLogs = ref []
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase
            let auditLog = this.CreateAuditLog "Account" "1001" AuditAction.Create "user1" "ユーザー1"

            // When
            let! recorded = service.RecordAsync(auditLog)

            // Then
            recorded.Id |> should not' (equal None)
            recorded.EntityType |> should equal "Account"
            recorded.EntityId |> should equal "1001"
            recorded.Action |> should equal AuditAction.Create
            (!auditLogs).Length |> should equal 1
        }

    [<Fact>]
    member this.``エンティティ別の監査ログを取得できる``() =
        task {
            // Given
            let auditLogs = ref (this.CreateInitialAuditLogs())
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase

            // When
            let! logs = service.GetByEntityAsync("Account", "1001")

            // Then
            logs.Length |> should equal 2
            logs |> List.forall (fun l -> l.EntityType = "Account" && l.EntityId = "1001")
                |> should equal true
        }

    [<Fact>]
    member this.``ユーザー別の監査ログを期間指定で取得できる``() =
        task {
            // Given
            let auditLogs = ref (this.CreateInitialAuditLogs())
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase

            // When
            let! logs = service.GetByUserAsync("user1", DateTime(2024, 1, 1), DateTime(2024, 1, 31))

            // Then
            logs.Length |> should equal 3
            logs |> List.forall (fun l -> l.UserId = "user1")
                |> should equal true
        }

    [<Fact>]
    member this.``期間内のすべての監査ログを取得できる``() =
        task {
            // Given
            let auditLogs = ref (this.CreateInitialAuditLogs())
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase

            // When
            let! logs = service.GetByDateRangeAsync(DateTime(2024, 1, 16), DateTime(2024, 1, 17, 23, 59, 59))

            // Then
            logs.Length |> should equal 2
        }

    [<Fact>]
    member this.``アクション種別で監査ログを検索できる``() =
        task {
            // Given
            let auditLogs = ref (this.CreateInitialAuditLogs())
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase

            // When
            let! logs = service.GetByActionAsync("CREATE", DateTime(2024, 1, 1), DateTime(2024, 1, 31))

            // Then
            logs.Length |> should equal 2
            logs |> List.forall (fun l -> l.Action = AuditAction.Create)
                |> should equal true
        }

    [<Fact>]
    member this.``DELETE操作の監査ログを検索できる``() =
        task {
            // Given
            let auditLogs = ref (this.CreateInitialAuditLogs())
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase

            // When
            let! logs = service.GetByActionAsync("DELETE", DateTime(2024, 1, 1), DateTime(2024, 1, 31))

            // Then
            logs.Length |> should equal 1
            logs.Head.EntityType |> should equal "Account"
            logs.Head.EntityId |> should equal "1002"
        }

    [<Fact>]
    member this.``存在しないエンティティの監査ログは空リストを返す``() =
        task {
            // Given
            let auditLogs = ref (this.CreateInitialAuditLogs())
            let repository = MockAuditLogRepository(auditLogs)
            let service = AuditLogService(repository) :> IAuditLogUseCase

            // When
            let! logs = service.GetByEntityAsync("Account", "9999")

            // Then
            logs.Length |> should equal 0
        }

    [<Fact>]
    member this.``AuditLogモジュールのcreate関数でCREATE操作のログを作成できる``() =
        // Given & When
        let auditLog = AuditLog.create "Account" "1001" "user1" "ユーザー1" "{\"name\": \"現金\"}" (Some "192.168.1.1")

        // Then
        auditLog.EntityType |> should equal "Account"
        auditLog.EntityId |> should equal "1001"
        auditLog.Action |> should equal AuditAction.Create
        auditLog.UserId |> should equal "user1"
        auditLog.UserName |> should equal "ユーザー1"
        auditLog.OldValues |> should equal None
        auditLog.NewValues |> should equal (Some "{\"name\": \"現金\"}")
        auditLog.IpAddress |> should equal (Some "192.168.1.1")

    [<Fact>]
    member _.``AuditLogモジュールのcreateForUpdate関数でUPDATE操作のログを作成できる``() =
        // Given & When
        let auditLog = AuditLog.createForUpdate
                        "Account" "1001" "user1" "ユーザー1"
                        "{\"name\": \"現金\"}"
                        "{\"name\": \"現金及び預金\"}"
                        (Some "192.168.1.1")

        // Then
        auditLog.Action |> should equal AuditAction.Update
        auditLog.OldValues |> should equal (Some "{\"name\": \"現金\"}")
        auditLog.NewValues |> should equal (Some "{\"name\": \"現金及び預金\"}")

    [<Fact>]
    member _.``AuditLogモジュールのcreateForDelete関数でDELETE操作のログを作成できる``() =
        // Given & When
        let auditLog = AuditLog.createForDelete
                        "Account" "1001" "user1" "ユーザー1"
                        "{\"name\": \"現金\"}"
                        (Some "科目統合のため")
                        (Some "192.168.1.1")

        // Then
        auditLog.Action |> should equal AuditAction.Delete
        auditLog.OldValues |> should equal (Some "{\"name\": \"現金\"}")
        auditLog.NewValues |> should equal None
        auditLog.Reason |> should equal (Some "科目統合のため")

    [<Fact>]
    member _.``AuditLogモジュールのgetSummary関数でサマリーを取得できる``() =
        // Given
        let auditLog = AuditLog.create "Account" "1001" "user1" "ユーザー1" "{}" None

        // When
        let summary = AuditLog.getSummary auditLog

        // Then
        summary |> should equal "Account 1001 を作成"

    [<Fact>]
    member _.``AuditActionモジュールのtoCode関数でコード変換できる``() =
        AuditAction.toCode AuditAction.Create |> should equal "CREATE"
        AuditAction.toCode AuditAction.Update |> should equal "UPDATE"
        AuditAction.toCode AuditAction.Delete |> should equal "DELETE"

    [<Fact>]
    member _.``AuditActionモジュールのfromCode関数でコードから変換できる``() =
        AuditAction.fromCode "CREATE" |> should equal (Some AuditAction.Create)
        AuditAction.fromCode "UPDATE" |> should equal (Some AuditAction.Update)
        AuditAction.fromCode "DELETE" |> should equal (Some AuditAction.Delete)
        AuditAction.fromCode "INVALID" |> should equal None

    [<Fact>]
    member _.``AuditActionモジュールのgetDisplayName関数で表示名を取得できる``() =
        AuditAction.getDisplayName AuditAction.Create |> should equal "作成"
        AuditAction.getDisplayName AuditAction.Update |> should equal "更新"
        AuditAction.getDisplayName AuditAction.Delete |> should equal "削除"
