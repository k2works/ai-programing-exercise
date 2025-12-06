module AccountingSystem.Tests.Domain.AuditLogTest

open System
open Xunit
open FsUnit.Xunit
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models

/// <summary>
/// AuditAction Discriminated Union のテスト
/// </summary>
module AuditActionTests =

    [<Fact>]
    let ``toCode - Create は CREATE を返す`` () =
        AuditAction.toCode AuditAction.Create |> should equal "CREATE"

    [<Fact>]
    let ``toCode - Update は UPDATE を返す`` () =
        AuditAction.toCode AuditAction.Update |> should equal "UPDATE"

    [<Fact>]
    let ``toCode - Delete は DELETE を返す`` () =
        AuditAction.toCode AuditAction.Delete |> should equal "DELETE"

    [<Fact>]
    let ``fromCode - CREATE は Some Create を返す`` () =
        AuditAction.fromCode "CREATE" |> should equal (Some AuditAction.Create)

    [<Fact>]
    let ``fromCode - UPDATE は Some Update を返す`` () =
        AuditAction.fromCode "UPDATE" |> should equal (Some AuditAction.Update)

    [<Fact>]
    let ``fromCode - DELETE は Some Delete を返す`` () =
        AuditAction.fromCode "DELETE" |> should equal (Some AuditAction.Delete)

    [<Fact>]
    let ``fromCode - 小文字でも変換できる`` () =
        AuditAction.fromCode "create" |> should equal (Some AuditAction.Create)
        AuditAction.fromCode "update" |> should equal (Some AuditAction.Update)
        AuditAction.fromCode "delete" |> should equal (Some AuditAction.Delete)

    [<Fact>]
    let ``fromCode - 無効なコードは None を返す`` () =
        AuditAction.fromCode "INVALID" |> should equal None
        AuditAction.fromCode "" |> should equal None
        AuditAction.fromCode "INSERT" |> should equal None

    [<Fact>]
    let ``getDisplayName - Create は 作成 を返す`` () =
        AuditAction.getDisplayName AuditAction.Create |> should equal "作成"

    [<Fact>]
    let ``getDisplayName - Update は 更新 を返す`` () =
        AuditAction.getDisplayName AuditAction.Update |> should equal "更新"

    [<Fact>]
    let ``getDisplayName - Delete は 削除 を返す`` () =
        AuditAction.getDisplayName AuditAction.Delete |> should equal "削除"

/// <summary>
/// AuditLog ドメインモデルのテスト
/// </summary>
module AuditLogTests =

    [<Fact>]
    let ``create - CREATE操作の監査ログを作成できる`` () =
        let log = AuditLog.create
                    "Account" "1001" "user1" "テストユーザー"
                    "{\"name\": \"現金\"}" (Some "192.168.1.1")

        log.EntityType |> should equal "Account"
        log.EntityId |> should equal "1001"
        log.Action |> should equal AuditAction.Create
        log.UserId |> should equal "user1"
        log.UserName |> should equal "テストユーザー"
        log.OldValues |> should equal None
        log.NewValues |> should equal (Some "{\"name\": \"現金\"}")
        log.Changes |> should equal (Some "{\"name\": \"現金\"}")
        log.IpAddress |> should equal (Some "192.168.1.1")
        log.Id |> should equal None

    [<Fact>]
    let ``create - IPアドレスなしでも作成できる`` () =
        let log = AuditLog.create
                    "Journal" "J-001" "user2" "ユーザー2"
                    "{}" None

        log.IpAddress |> should equal None

    [<Fact>]
    let ``createForUpdate - UPDATE操作の監査ログを作成できる`` () =
        let log = AuditLog.createForUpdate
                    "Account" "1001" "user1" "テストユーザー"
                    "{\"name\": \"現金\"}"
                    "{\"name\": \"現金及び預金\"}"
                    (Some "10.0.0.1")

        log.Action |> should equal AuditAction.Update
        log.OldValues |> should equal (Some "{\"name\": \"現金\"}")
        log.NewValues |> should equal (Some "{\"name\": \"現金及び預金\"}")
        log.Changes |> should equal None

    [<Fact>]
    let ``createForDelete - DELETE操作の監査ログを作成できる`` () =
        let log = AuditLog.createForDelete
                    "Account" "1002" "admin" "管理者"
                    "{\"name\": \"廃止科目\"}"
                    (Some "科目統合のため")
                    (Some "172.16.0.1")

        log.Action |> should equal AuditAction.Delete
        log.OldValues |> should equal (Some "{\"name\": \"廃止科目\"}")
        log.NewValues |> should equal None
        log.Reason |> should equal (Some "科目統合のため")

    [<Fact>]
    let ``createForDelete - 理由なしでも作成できる`` () =
        let log = AuditLog.createForDelete
                    "Journal" "J-002" "user1" "ユーザー"
                    "{}" None None

        log.Reason |> should equal None

    [<Fact>]
    let ``getSummary - CREATE操作のサマリーを取得できる`` () =
        let log = AuditLog.create "Account" "1001" "user1" "ユーザー" "{}" None
        AuditLog.getSummary log |> should equal "Account 1001 を作成"

    [<Fact>]
    let ``getSummary - UPDATE操作のサマリーを取得できる`` () =
        let log = AuditLog.createForUpdate
                    "Journal" "J-2024-0001" "user1" "ユーザー"
                    "{}" "{}" None
        AuditLog.getSummary log |> should equal "Journal J-2024-0001 を更新"

    [<Fact>]
    let ``getSummary - DELETE操作のサマリーを取得できる`` () =
        let log = AuditLog.createForDelete
                    "Account" "9999" "admin" "管理者"
                    "{}" None None
        AuditLog.getSummary log |> should equal "Account 9999 を削除"

    [<Fact>]
    let ``Timestamp は UTC で記録される`` () =
        let beforeCreate = DateTime.UtcNow
        let log = AuditLog.create "Test" "1" "user" "ユーザー" "{}" None
        let afterCreate = DateTime.UtcNow

        log.Timestamp |> should be (greaterThanOrEqualTo beforeCreate)
        log.Timestamp |> should be (lessThanOrEqualTo afterCreate)

    [<Fact>]
    let ``AuditLog レコードは不変である`` () =
        let log = AuditLog.create "Account" "1001" "user1" "ユーザー" "{}" None

        // with 式で新しいレコードを作成
        let updatedLog = { log with Reason = Some "テスト理由" }

        // 元のレコードは変更されない
        log.Reason |> should equal None
        updatedLog.Reason |> should equal (Some "テスト理由")

    [<Fact>]
    let ``AuditLog レコードの構造的等価性`` () =
        let log1 = {
            Id = Some 1L
            EntityType = "Account"
            EntityId = "1001"
            Action = AuditAction.Create
            UserId = "user1"
            UserName = "ユーザー1"
            Timestamp = DateTime(2024, 1, 15, 10, 0, 0)
            OldValues = None
            NewValues = Some "{}"
            Changes = None
            Reason = None
            IpAddress = None
            UserAgent = None
        }

        let log2 = {
            Id = Some 1L
            EntityType = "Account"
            EntityId = "1001"
            Action = AuditAction.Create
            UserId = "user1"
            UserName = "ユーザー1"
            Timestamp = DateTime(2024, 1, 15, 10, 0, 0)
            OldValues = None
            NewValues = Some "{}"
            Changes = None
            Reason = None
            IpAddress = None
            UserAgent = None
        }

        log1 |> should equal log2

    [<Fact>]
    let ``異なる AuditAction は等しくない`` () =
        AuditAction.Create |> should not' (equal AuditAction.Update)
        AuditAction.Update |> should not' (equal AuditAction.Delete)
        AuditAction.Delete |> should not' (equal AuditAction.Create)

    [<Fact>]
    let ``5W1H情報を完全に記録できる`` () =
        // Who (誰が)
        let userId = "user123"
        let userName = "山田太郎"

        // When (いつ)
        let timestamp = DateTime(2024, 6, 15, 14, 30, 0)

        // What (何を)
        let entityType = "Account"
        let entityId = "1001"

        // Which (どの操作)
        let action = AuditAction.Update

        // Why (なぜ)
        let reason = "月次決算処理"

        // How (どう変更)
        let oldValues = "{\"balance\": 100000}"
        let newValues = "{\"balance\": 150000}"

        let log = {
            Id = None
            EntityType = entityType
            EntityId = entityId
            Action = action
            UserId = userId
            UserName = userName
            Timestamp = timestamp
            OldValues = Some oldValues
            NewValues = Some newValues
            Changes = None
            Reason = Some reason
            IpAddress = Some "192.168.1.100"
            UserAgent = Some "AccountingApp/2.0"
        }

        // 5W1H が正しく記録されていることを検証
        log.UserId |> should equal userId          // Who
        log.UserName |> should equal userName      // Who
        log.Timestamp |> should equal timestamp    // When
        log.EntityType |> should equal entityType  // What
        log.EntityId |> should equal entityId      // What
        log.Action |> should equal action          // Which
        log.Reason |> should equal (Some reason)   // Why
        log.OldValues |> should equal (Some oldValues)  // How
        log.NewValues |> should equal (Some newValues)  // How
