namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Application.Port.Out

/// <summary>
/// 仕訳クエリサービス（CQRS 読み取り側）
/// </summary>
type JournalEntryQueryService(readModelRepository: IJournalEntryReadModelRepository) =

    /// <summary>
    /// 仕訳を取得
    /// </summary>
    member _.GetJournalEntryAsync(id: string) : Task<Result<JournalEntryReadModel, string>> =
        task {
            let! entityOption = readModelRepository.SelectByIdAsync id

            match entityOption with
            | Some entity -> return Ok entity
            | None -> return Error $"仕訳が見つかりません: {id}"
        }

    /// <summary>
    /// 期間で仕訳を検索
    /// </summary>
    member _.GetJournalEntriesByDateRangeAsync
        (startDate: DateTime)
        (endDate: DateTime)
        : Task<JournalEntryReadModel list> =
        task {
            return! readModelRepository.SelectByDateRangeAsync startDate endDate
        }

    /// <summary>
    /// 仕訳明細を取得
    /// </summary>
    member _.GetJournalEntryLinesAsync
        (journalEntryId: string)
        : Task<JournalEntryLineReadModel list> =
        task {
            return! readModelRepository.SelectLinesByJournalEntryIdAsync journalEntryId
        }

    /// <summary>
    /// すべての仕訳を取得
    /// </summary>
    member _.GetAllJournalEntriesAsync () : Task<JournalEntryReadModel list> =
        task {
            return! readModelRepository.SelectAllAsync ()
        }
