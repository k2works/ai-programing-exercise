namespace FinancialAccounting.Infrastructure.Web.Dtos

open System
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.In

/// <summary>
/// 仕訳明細リクエスト DTO
/// </summary>
[<CLIMutable>]
type JournalEntryRequestDto = {
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// 仕訳リクエスト DTO
/// </summary>
[<CLIMutable>]
type JournalRequestDto = {
    JournalDate: DateTime
    Description: string
    FiscalYear: int
    Entries: JournalEntryRequestDto[]
}

/// <summary>
/// 仕訳明細レスポンス DTO
/// </summary>
[<CLIMutable>]
type JournalEntryResponseDto = {
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// 仕訳レスポンス DTO
/// </summary>
[<CLIMutable>]
type JournalResponseDto = {
    JournalId: int
    JournalDate: DateTime
    Description: string
    FiscalYear: int
    Entries: JournalEntryResponseDto[]
}

module JournalDto =
    /// <summary>
    /// リクエスト DTO を UseCase リクエストに変換
    /// </summary>
    let toCreateRequest (dto: JournalRequestDto) : CreateJournalRequest =
        {
            JournalDate = dto.JournalDate
            Description = dto.Description
            FiscalYear = dto.FiscalYear
            Entries = dto.Entries
                |> Array.toList
                |> List.map (fun e ->
                    {
                        AccountCode = e.AccountCode
                        DebitAmount = e.DebitAmount
                        CreditAmount = e.CreditAmount
                        Description = e.Description
                    } : CreateJournalEntryRequest)
        }

    /// <summary>
    /// ドメインエンティティをレスポンス DTO に変換
    /// </summary>
    let toResponse (journal: Journal) : JournalResponseDto =
        {
            JournalId = journal.JournalId |> Option.defaultValue 0
            JournalDate = journal.JournalDate
            Description = journal.Description
            FiscalYear = journal.FiscalYear
            Entries = journal.Entries
                |> List.map (fun e ->
                    {
                        AccountCode = e.AccountCode
                        DebitAmount = e.DebitAmount
                        CreditAmount = e.CreditAmount
                        Description = e.Description
                    })
                |> List.toArray
        }
