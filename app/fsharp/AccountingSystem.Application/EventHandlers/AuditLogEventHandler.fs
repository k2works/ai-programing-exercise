namespace AccountingSystem.Application.EventHandlers

open System
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
                            AuditLog.create
                                "JournalEntry"
                                data.JournalEntryId
                                data.UserId
                                data.UserId
                                $"仕訳作成: {data.Description}"
                                None

                        | JournalEntryApproved data ->
                            AuditLog.createForUpdate
                                "JournalEntry"
                                data.JournalEntryId
                                data.UserId
                                data.UserId
                                "Draft"
                                $"Approved: {data.ApprovalComment}"
                                None

                        | JournalEntryDeleted data ->
                            AuditLog.createForDelete
                                "JournalEntry"
                                data.JournalEntryId
                                data.UserId
                                data.UserId
                                ""
                                (Some $"仕訳削除: {data.Reason}")
                                None

                    let! _ = auditLogRepository.InsertAsync auditLog

                    return Ok ()
                with
                | ex ->
                    return Error $"監査ログ記録エラー: {ex.Message}"
            }
