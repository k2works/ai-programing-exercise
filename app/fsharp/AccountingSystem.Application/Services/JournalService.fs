namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Application.Exceptions
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Domain.Models.Journal
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳サービス実装
/// ビジネスロジックとトランザクション管理
/// </summary>
type JournalService(journalRepository: IJournalRepository) =

    /// <summary>
    /// 貸借一致の検証
    /// </summary>
    let validateDoubleEntry (journal: Journal) : Result<unit, string> =
        let totalDebit = Journal.sumDebit journal |> Money.toDecimal
        let totalCredit = Journal.sumCredit journal |> Money.toDecimal

        if totalDebit <> totalCredit then
            Error $"貸借が一致していません。借方合計: {totalDebit}, 貸方合計: {totalCredit}"
        else
            Ok ()

    /// <summary>
    /// 仕訳の検証
    /// </summary>
    let validateJournal (journal: Journal) : Result<unit, string> =
        // 1. 明細が存在するか
        if journal.Lines.IsEmpty then
            Error "仕訳明細が必要です"
        // 2. 各明細に項目が存在するか
        elif journal.Lines |> List.exists (fun line -> line.Items.IsEmpty) then
            Error "各仕訳明細には少なくとも1つの項目が必要です"
        else
            // 3. 貸借一致の検証
            validateDoubleEntry journal

    interface IJournalUseCase with

        member _.GetJournalByVoucherNumberAsync(voucherNumber: int) : Task<Journal> =
            task {
                let! journal = journalRepository.FindByVoucherNumberAsync(string voucherNumber)

                return
                    journal
                    |> Option.defaultWith (fun () ->
                        raise (JournalNotFoundException(voucherNumber)))
            }

        member _.GetJournalsByDateRangeAsync(fromDate: DateTime, toDate: DateTime) : Task<Journal list> =
            task {
                if fromDate > toDate then
                    raise (ArgumentException("開始日は終了日より前である必要があります"))

                return! journalRepository.FindByDateRangeAsync(fromDate, toDate)
            }

        member _.CreateJournalAsync(journal: Journal) : Task<Journal> =
            task {
                // ビジネスルール検証
                match validateJournal journal with
                | Error msg -> raise (InvalidJournalEntryException(msg))
                | Ok () -> ()

                // 重複チェック
                let! existing = journalRepository.FindByVoucherNumberAsync(string journal.VoucherNumber.Number)

                match existing with
                | Some _ ->
                    return raise (BusinessRuleViolationException($"伝票番号 {journal.VoucherNumber.Number} は既に存在します"))
                | None ->
                    let! _ = journalRepository.SaveAsync(journal)
                    let! created = journalRepository.FindByVoucherNumberAsync(string journal.VoucherNumber.Number)
                    return
                        created
                        |> Option.defaultWith (fun () ->
                            raise (JournalNotFoundException(journal.VoucherNumber.Number)))
            }

        member _.UpdateJournalAsync(voucherNumber: int) (journal: Journal) : Task<Journal> =
            task {
                // 存在チェック
                let! existing = journalRepository.FindByVoucherNumberAsync(string voucherNumber)

                match existing with
                | None ->
                    return raise (JournalNotFoundException(voucherNumber))
                | Some _ ->
                    // ビジネスルール検証
                    match validateJournal journal with
                    | Error msg -> raise (InvalidJournalEntryException(msg))
                    | Ok () -> ()

                    // 伝票番号を保持して更新
                    let updatedJournal = { journal with VoucherNumber = VoucherNumber.Create(string voucherNumber) }
                    let! _ = journalRepository.UpdateAsync(updatedJournal)
                    let! updated = journalRepository.FindByVoucherNumberAsync(string voucherNumber)
                    return
                        updated
                        |> Option.defaultWith (fun () ->
                            raise (JournalNotFoundException(voucherNumber)))
            }

        member _.DeleteJournalAsync(voucherNumber: int) : Task<unit> =
            task {
                let! existing = journalRepository.FindByVoucherNumberAsync(string voucherNumber)

                match existing with
                | None ->
                    raise (JournalNotFoundException(voucherNumber))
                | Some _ ->
                    let! _ = journalRepository.DeleteAsync(string voucherNumber)
                    return ()
            }
