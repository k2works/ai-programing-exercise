namespace AccountingSystem.Application.EventHandlers

open System
open System.Text.Json
open System.Threading.Tasks
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// 監査ログイベントハンドラー
/// イベントを受信して監査ログを記録する
/// </summary>
type AuditLogEventHandler(auditLogRepository: IAuditLogRepository) =

    /// JSON シリアライズオプション
    let jsonOptions = JsonSerializerOptions(
        WriteIndented = false,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    )

    /// オブジェクトを JSON 文字列に変換
    let toJson obj = JsonSerializer.Serialize(obj, jsonOptions)

    interface IJournalEntryEventHandler with
        /// <summary>
        /// ハンドラーが処理するイベントタイプ
        /// </summary>
        member _.EventTypes =
            [ "JournalEntryCreated"; "JournalEntryApproved"; "JournalEntryDeleted" ]

        /// <summary>
        /// イベントを処理
        /// </summary>
        member _.HandleAsync(event: JournalEntryEvent) : Task<Result<unit, string>> =
            task {
                try
                    let auditLog =
                        match event with
                        | JournalEntryCreated data ->
                            let changesJson = toJson {|
                                action = "create"
                                description = data.Description
                                entryDate = data.EntryDate.ToString("yyyy-MM-dd")
                                lineItemCount = data.LineItems.Length
                            |}
                            AuditLog.create
                                "JournalEntry"
                                data.JournalEntryId
                                data.UserId
                                data.UserId
                                changesJson
                                None

                        | JournalEntryApproved data ->
                            let oldValuesJson = toJson {| status = "Draft" |}
                            let newValuesJson = toJson {|
                                status = "Approved"
                                approvedBy = data.ApprovedBy
                                approvalComment = data.ApprovalComment
                            |}
                            AuditLog.createForUpdate
                                "JournalEntry"
                                data.JournalEntryId
                                data.UserId
                                data.UserId
                                oldValuesJson
                                newValuesJson
                                None

                        | JournalEntryDeleted data ->
                            let oldValuesJson = toJson {| status = "Deleted" |}
                            AuditLog.createForDelete
                                "JournalEntry"
                                data.JournalEntryId
                                data.UserId
                                data.UserId
                                oldValuesJson
                                (Some data.Reason)
                                None

                    let! _ = auditLogRepository.InsertAsync auditLog

                    return Ok ()
                with
                | ex ->
                    return Error $"監査ログ記録エラー: {ex.Message}"
            }
