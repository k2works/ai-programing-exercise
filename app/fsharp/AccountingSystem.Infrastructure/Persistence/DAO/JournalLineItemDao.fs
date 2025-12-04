module AccountingSystem.Infrastructure.DAO.JournalLineItemDao

open System
open AccountingSystem.Domain.Models.JournalLineItem
open AccountingSystem.Domain.Types

/// <summary>
/// 仕訳貸借明細テーブル（3層構造3層目）のDAO
/// </summary>
[<CLIMutable>]
type JournalLineItemDao = {
    VoucherNumber: string
    LineNumber: int16
    DebitCreditType: string
    CurrencyCode: string
    ExchangeRate: decimal
    DepartmentCode: string
    ProjectCode: string
    AccountCode: string
    SubAccountCode: string
    Amount: decimal
    BaseAmount: decimal
    TaxCategory: string
    TaxRate: Nullable<int16>
    TaxCalculationType: string
    DueDate: Nullable<DateTime>
    CashFlowFlag: int16
    SegmentCode: string
    CounterAccountCode: string
    CounterSubAccountCode: string
    MemoCode: string
    MemoContent: string
    CreatedAt: DateTime
    UpdatedAt: DateTime
}

module JournalLineItemDao =
    /// ドメインモデルからDAOへ変換
    let fromDomain (item: JournalLineItem) : JournalLineItemDao =
        {
            VoucherNumber = item.VoucherNumber.Number
            LineNumber = int16 item.LineNumber
            DebitCreditType = item.DebitCreditType.ToCode()
            CurrencyCode = item.Amount.Currency.Code
            ExchangeRate = item.ExchangeRate
            DepartmentCode = item.DepartmentCode |> Option.defaultValue null
            ProjectCode = item.ProjectCode |> Option.defaultValue null
            AccountCode = item.AccountCode.Code
            SubAccountCode = item.SubAccountCode |> Option.defaultValue null
            Amount = item.Amount.Amount
            BaseAmount = item.BaseAmount.Amount
            TaxCategory = item.TaxCategory |> Option.defaultValue null
            TaxRate =
                item.TaxRate
                |> Option.map (int16 >> Nullable)
                |> Option.defaultValue (Nullable())
            TaxCalculationType = item.TaxCalculationType |> Option.defaultValue null
            DueDate =
                item.DueDate
                |> Option.map Nullable
                |> Option.defaultValue (Nullable())
            CashFlowFlag = if item.IsCashFlow then 1s else 0s
            SegmentCode = item.SegmentCode |> Option.defaultValue null
            CounterAccountCode = item.CounterAccountCode |> Option.defaultValue null
            CounterSubAccountCode = item.CounterSubAccountCode |> Option.defaultValue null
            MemoCode = item.MemoCode |> Option.defaultValue null
            MemoContent = item.MemoContent |> Option.defaultValue null
            CreatedAt = DateTime.UtcNow
            UpdatedAt = DateTime.UtcNow
        }

    /// DAOからドメインモデルへ変換
    let toDomain (dao: JournalLineItemDao) : JournalLineItem =
        let currencyCode = CurrencyCode.Create(dao.CurrencyCode)
        {
            VoucherNumber = VoucherNumber.Create(dao.VoucherNumber)
            LineNumber = int dao.LineNumber
            DebitCreditType =
                DebitCreditType.FromCode(dao.DebitCreditType)
                |> Option.defaultValue Debit
            ExchangeRate = dao.ExchangeRate
            DepartmentCode = if isNull dao.DepartmentCode then None else Some dao.DepartmentCode
            ProjectCode = if isNull dao.ProjectCode then None else Some dao.ProjectCode
            AccountCode = AccountCode.Create(dao.AccountCode)
            SubAccountCode = if isNull dao.SubAccountCode then None else Some dao.SubAccountCode
            Amount = CurrencyAmount.Create(currencyCode, dao.Amount)
            BaseAmount = Money.Create(dao.BaseAmount)
            TaxCategory = if isNull dao.TaxCategory then None else Some dao.TaxCategory
            TaxRate = if dao.TaxRate.HasValue then Some (int dao.TaxRate.Value) else None
            TaxCalculationType = if isNull dao.TaxCalculationType then None else Some dao.TaxCalculationType
            DueDate = if dao.DueDate.HasValue then Some dao.DueDate.Value else None
            IsCashFlow = dao.CashFlowFlag = 1s
            SegmentCode = if isNull dao.SegmentCode then None else Some dao.SegmentCode
            CounterAccountCode = if isNull dao.CounterAccountCode then None else Some dao.CounterAccountCode
            CounterSubAccountCode = if isNull dao.CounterSubAccountCode then None else Some dao.CounterSubAccountCode
            MemoCode = if isNull dao.MemoCode then None else Some dao.MemoCode
            MemoContent = if isNull dao.MemoContent then None else Some dao.MemoContent
        }
