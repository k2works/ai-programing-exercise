namespace FinancialAccounting.Application.UseCases

open System.Threading.Tasks
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Application.Ports.Out

/// <summary>
/// 仕訳ユースケースの実装
/// </summary>
type JournalUseCase(repository: IJournalRepository, eventPublisher: IJournalEventPublisher) =

    /// <summary>
    /// リクエストからドメインエンティティに変換
    /// </summary>
    let toJournalEntry (req: CreateJournalEntryRequest) : JournalEntry =
        {
            JournalId = None
            AccountCode = req.AccountCode
            DebitAmount = req.DebitAmount
            CreditAmount = req.CreditAmount
            Description = req.Description
        }

    /// <summary>
    /// リクエストからJournalエンティティに変換
    /// </summary>
    let toJournal (req: CreateJournalRequest) : Journal =
        {
            JournalId = None
            JournalDate = req.JournalDate
            Description = req.Description
            FiscalYear = req.FiscalYear
            Entries = req.Entries |> List.map toJournalEntry
        }

    interface IJournalUseCase with
        member _.CreateJournalAsync(request: CreateJournalRequest) : Task<Result<Journal, string>> =
            task {
                // 明細の存在チェック
                if request.Entries.IsEmpty then
                    return Error "仕訳明細が必要です"
                else
                    let journal = toJournal request

                    // 貸借一致の検証
                    match Journal.validateBalance journal with
                    | Error msg ->
                        return Error msg
                    | Ok () ->
                        let! saved = repository.SaveAsync(journal)

                        // 仕訳作成イベントを発行
                        do! eventPublisher.PublishJournalCreatedAsync(saved)

                        return Ok saved
            }

        member _.GetJournalByIdAsync(journalId: int) : Task<Journal option> =
            repository.GetByIdAsync(journalId)

        member _.GetJournalsByFiscalYearAsync(fiscalYear: int) : Task<Journal list> =
            repository.GetByFiscalYearAsync(fiscalYear)
