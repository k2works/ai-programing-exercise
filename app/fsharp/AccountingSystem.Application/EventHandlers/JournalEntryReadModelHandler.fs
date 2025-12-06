namespace AccountingSystem.Application.EventHandlers

open System
open System.Threading.Tasks
open AccountingSystem.Domain.Events
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out

/// <summary>
/// 仕訳 Read Model 更新イベントハンドラー
/// イベントを受信して Read Model を更新する（CQRS パターン）
/// </summary>
type JournalEntryReadModelHandler(readModelRepository: IJournalEntryReadModelRepository) =

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
                    match event with
                    | JournalEntryCreated data ->
                        // Read Model に仕訳を挿入
                        do! readModelRepository.InsertJournalEntryAsync
                                data.JournalEntryId
                                data.EntryDate
                                data.Description
                                "Draft"
                                false
                                data.OccurredAt
                                data.OccurredAt
                                None
                                None

                        // 明細を挿入
                        for lineItem in data.LineItems do
                            do! readModelRepository.InsertJournalEntryLineAsync
                                    data.JournalEntryId
                                    lineItem.AccountCode
                                    (lineItem.DebitCredit.ToCode())
                                    lineItem.Amount

                        return Ok ()

                    | JournalEntryApproved data ->
                        // ステータスを承認済みに更新
                        do! readModelRepository.UpdateJournalEntryStatusAsync
                                data.JournalEntryId
                                "Approved"
                                data.OccurredAt
                                data.ApprovedBy
                                data.ApprovalComment

                        return Ok ()

                    | JournalEntryDeleted data ->
                        // 削除済みフラグを設定
                        do! readModelRepository.MarkAsDeletedAsync
                                data.JournalEntryId
                                data.OccurredAt

                        return Ok ()
                with
                | ex ->
                    return Error $"Read Model 更新エラー: {ex.Message}"
            }
