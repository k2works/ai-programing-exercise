namespace AccountingSystem.Infrastructure.Web.Dtos

open System
open System.ComponentModel.DataAnnotations
open AccountingSystem.Domain.Types
open AccountingSystem.Domain.Models.JournalLineItem
open AccountingSystem.Domain.Models.JournalLine
open AccountingSystem.Domain.Models.Journal

/// <summary>
/// 仕訳明細項目リクエスト DTO
/// </summary>
[<CLIMutable>]
type JournalLineItemRequest = {
    [<Required(ErrorMessage = "貸借区分は必須です")>]
    DebitCreditType: string

    [<Required(ErrorMessage = "勘定科目コードは必須です")>]
    AccountCode: string

    SubAccountCode: string

    DepartmentCode: string

    ProjectCode: string

    [<Required(ErrorMessage = "金額は必須です")>]
    [<Range(0.0, 999999999999.99, ErrorMessage = "金額は0以上である必要があります")>]
    Amount: decimal

    CurrencyCode: string

    ExchangeRate: decimal

    TaxCategory: string

    TaxRate: Nullable<int>

    TaxCalculationType: string

    DueDate: Nullable<DateTime>

    IsCashFlow: bool

    SegmentCode: string

    CounterAccountCode: string

    CounterSubAccountCode: string

    MemoCode: string

    MemoContent: string
}

/// <summary>
/// 仕訳明細リクエスト DTO
/// </summary>
[<CLIMutable>]
type JournalLineRequest = {
    LineNumber: int

    [<Required(ErrorMessage = "摘要は必須です")>]
    Description: string

    [<Required(ErrorMessage = "明細項目は必須です")>]
    Items: JournalLineItemRequest array
}

/// <summary>
/// 仕訳リクエスト DTO
/// </summary>
[<CLIMutable>]
type JournalRequest = {
    [<Required(ErrorMessage = "伝票番号は必須です")>]
    VoucherNumber: int

    [<Required(ErrorMessage = "起票日は必須です")>]
    PostingDate: DateTime

    EntryDate: DateTime

    SettlementFlag: int

    IsSingleEntry: bool

    VoucherType: string

    IsRecurring: bool

    EmployeeCode: string

    DepartmentCode: string

    RedSlipFlag: int

    RedBlackVoucherNumber: Nullable<int>

    [<Required(ErrorMessage = "仕訳明細は必須です")>]
    Lines: JournalLineRequest array
}

module JournalRequest =
    /// <summary>
    /// Domain Model への変換
    /// </summary>
    let toDomain (request: JournalRequest) : Journal =
        let voucherType =
            match request.VoucherType with
            | "RECEIPT" -> VoucherType.Receipt
            | "PAYMENT" -> VoucherType.Payment
            | "TRANSFER" -> VoucherType.Transfer
            | _ -> VoucherType.Transfer

        let settlementFlag =
            match request.SettlementFlag with
            | 0 -> SettlementFlag.Normal
            | _ -> SettlementFlag.Settlement

        let redSlipFlag =
            match request.RedSlipFlag with
            | 0 -> RedSlipFlag.Normal
            | _ -> RedSlipFlag.RedSlip

        let voucherNumberStr = string request.VoucherNumber

        let lines =
            request.Lines
            |> Array.toList
            |> List.map (fun lineReq ->
                let items =
                    lineReq.Items
                    |> Array.toList
                    |> List.map (fun itemReq ->
                        let debitCreditType =
                            match itemReq.DebitCreditType with
                            | "D" | "借" -> DebitCreditType.Debit
                            | "C" | "貸" -> DebitCreditType.Credit
                            | _ -> DebitCreditType.Debit

                        let currencyCode =
                            match itemReq.CurrencyCode with
                            | null | "" | "JPY" -> CurrencyCode.JPY
                            | "USD" -> CurrencyCode.USD
                            | "EUR" -> CurrencyCode.EUR
                            | _ -> CurrencyCode.JPY

                        let exchangeRate =
                            if itemReq.ExchangeRate <= 0m then 1.0m else itemReq.ExchangeRate

                        let baseAmount = Money.Create(itemReq.Amount * exchangeRate)
                        let amount = CurrencyAmount.Create(currencyCode, itemReq.Amount)

                        {
                            VoucherNumber = VoucherNumber.Create(voucherNumberStr)
                            LineNumber = lineReq.LineNumber
                            DebitCreditType = debitCreditType
                            ExchangeRate = exchangeRate
                            DepartmentCode = if String.IsNullOrEmpty(itemReq.DepartmentCode) then None else Some itemReq.DepartmentCode
                            ProjectCode = if String.IsNullOrEmpty(itemReq.ProjectCode) then None else Some itemReq.ProjectCode
                            AccountCode = AccountCode.Create(itemReq.AccountCode)
                            SubAccountCode = if String.IsNullOrEmpty(itemReq.SubAccountCode) then None else Some itemReq.SubAccountCode
                            Amount = amount
                            BaseAmount = baseAmount
                            TaxCategory = if String.IsNullOrEmpty(itemReq.TaxCategory) then None else Some itemReq.TaxCategory
                            TaxRate = if itemReq.TaxRate.HasValue then Some itemReq.TaxRate.Value else None
                            TaxCalculationType = if String.IsNullOrEmpty(itemReq.TaxCalculationType) then None else Some itemReq.TaxCalculationType
                            DueDate = if itemReq.DueDate.HasValue then Some itemReq.DueDate.Value else None
                            IsCashFlow = itemReq.IsCashFlow
                            SegmentCode = if String.IsNullOrEmpty(itemReq.SegmentCode) then None else Some itemReq.SegmentCode
                            CounterAccountCode = if String.IsNullOrEmpty(itemReq.CounterAccountCode) then None else Some itemReq.CounterAccountCode
                            CounterSubAccountCode = if String.IsNullOrEmpty(itemReq.CounterSubAccountCode) then None else Some itemReq.CounterSubAccountCode
                            MemoCode = if String.IsNullOrEmpty(itemReq.MemoCode) then None else Some itemReq.MemoCode
                            MemoContent = if String.IsNullOrEmpty(itemReq.MemoContent) then None else Some itemReq.MemoContent
                        })

                {
                    VoucherNumber = VoucherNumber.Create(voucherNumberStr)
                    LineNumber = lineReq.LineNumber
                    Description = lineReq.Description
                    Items = items
                })

        {
            VoucherNumber = VoucherNumber.Create(voucherNumberStr)
            PostingDate = request.PostingDate
            EntryDate = if request.EntryDate = DateTime.MinValue then DateTime.Now else request.EntryDate
            SettlementFlag = settlementFlag
            IsSingleEntry = request.IsSingleEntry
            VoucherType = voucherType
            IsRecurring = request.IsRecurring
            EmployeeCode = if String.IsNullOrEmpty(request.EmployeeCode) then None else Some request.EmployeeCode
            DepartmentCode = if String.IsNullOrEmpty(request.DepartmentCode) then None else Some request.DepartmentCode
            RedSlipFlag = redSlipFlag
            RedBlackVoucherNumber = if request.RedBlackVoucherNumber.HasValue then Some request.RedBlackVoucherNumber.Value else None
            Lines = lines
        }

/// <summary>
/// 仕訳明細項目レスポンス DTO
/// </summary>
[<CLIMutable>]
type JournalLineItemResponse = {
    LineNumber: int
    DebitCreditType: string
    AccountCode: string
    SubAccountCode: string option
    DepartmentCode: string option
    ProjectCode: string option
    Amount: decimal
    CurrencyCode: string
    ExchangeRate: decimal
    BaseAmount: decimal
    TaxCategory: string option
    TaxRate: int option
    TaxCalculationType: string option
    DueDate: DateTime option
    IsCashFlow: bool
    SegmentCode: string option
    CounterAccountCode: string option
    CounterSubAccountCode: string option
    MemoCode: string option
    MemoContent: string option
}

/// <summary>
/// 仕訳明細レスポンス DTO
/// </summary>
[<CLIMutable>]
type JournalLineResponse = {
    VoucherNumber: string
    LineNumber: int
    Description: string
    Items: JournalLineItemResponse list
}

/// <summary>
/// 仕訳レスポンス DTO
/// </summary>
[<CLIMutable>]
type JournalResponse = {
    VoucherNumber: string
    PostingDate: DateTime
    EntryDate: DateTime
    SettlementFlag: int
    IsSingleEntry: bool
    VoucherType: string
    IsRecurring: bool
    EmployeeCode: string option
    DepartmentCode: string option
    RedSlipFlag: int
    RedBlackVoucherNumber: int option
    Lines: JournalLineResponse list
    TotalDebit: decimal
    TotalCredit: decimal
}

module JournalResponse =
    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    let from (journal: Journal) : JournalResponse =
        let voucherTypeStr =
            match journal.VoucherType with
            | VoucherType.Receipt -> "RECEIPT"
            | VoucherType.Payment -> "PAYMENT"
            | VoucherType.Transfer -> "TRANSFER"

        let settlementFlagInt =
            match journal.SettlementFlag with
            | SettlementFlag.Normal -> 0
            | SettlementFlag.Settlement -> 1

        let redSlipFlagInt =
            match journal.RedSlipFlag with
            | RedSlipFlag.Normal -> 0
            | RedSlipFlag.RedSlip -> 1

        let lines =
            journal.Lines
            |> List.map (fun line ->
                let items =
                    line.Items
                    |> List.map (fun item ->
                        let debitCreditStr =
                            match item.DebitCreditType with
                            | DebitCreditType.Debit -> "D"
                            | DebitCreditType.Credit -> "C"

                        let currencyCodeStr = item.Amount.Currency.Code

                        {
                            LineNumber = item.LineNumber
                            DebitCreditType = debitCreditStr
                            AccountCode = item.AccountCode.Code
                            SubAccountCode = item.SubAccountCode
                            DepartmentCode = item.DepartmentCode
                            ProjectCode = item.ProjectCode
                            Amount = item.Amount.Amount
                            CurrencyCode = currencyCodeStr
                            ExchangeRate = item.ExchangeRate
                            BaseAmount = Money.toDecimal item.BaseAmount
                            TaxCategory = item.TaxCategory
                            TaxRate = item.TaxRate
                            TaxCalculationType = item.TaxCalculationType
                            DueDate = item.DueDate
                            IsCashFlow = item.IsCashFlow
                            SegmentCode = item.SegmentCode
                            CounterAccountCode = item.CounterAccountCode
                            CounterSubAccountCode = item.CounterSubAccountCode
                            MemoCode = item.MemoCode
                            MemoContent = item.MemoContent
                        })

                {
                    VoucherNumber = line.VoucherNumber.Number
                    LineNumber = line.LineNumber
                    Description = line.Description
                    Items = items
                })

        {
            VoucherNumber = journal.VoucherNumber.Number
            PostingDate = journal.PostingDate
            EntryDate = journal.EntryDate
            SettlementFlag = settlementFlagInt
            IsSingleEntry = journal.IsSingleEntry
            VoucherType = voucherTypeStr
            IsRecurring = journal.IsRecurring
            EmployeeCode = journal.EmployeeCode
            DepartmentCode = journal.DepartmentCode
            RedSlipFlag = redSlipFlagInt
            RedBlackVoucherNumber = journal.RedBlackVoucherNumber
            Lines = lines
            TotalDebit = Journal.sumDebit journal |> Money.toDecimal
            TotalCredit = Journal.sumCredit journal |> Money.toDecimal
        }

    /// <summary>
    /// Domain Model リストからの変換
    /// </summary>
    let fromList (journals: Journal list) : JournalResponse list =
        journals |> List.map from
