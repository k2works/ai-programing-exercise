namespace AccountingSystem.Infrastructure.DAO

open AccountingSystem.Domain.Models

/// <summary>
/// 勘定科目 DAO（Dapper マッピング用）
/// </summary>
[<CLIMutable>]
type AccountDao = {
    AccountId: int
    AccountCode: string
    AccountName: string
    AccountNameKana: string
    AccountType: string
    IsSummaryAccount: bool
    BsplType: string
    TransactionElementType: string
    ExpenseType: string
    DisplayOrder: int
    IsAggregationTarget: bool
    TaxCode: string
    Balance: decimal
}

module AccountDao =
    /// DAO からドメインモデルへ変換
    let toDomain (dao: AccountDao) : Account =
        {
            AccountId = if dao.AccountId > 0 then Some dao.AccountId else None
            AccountCode = dao.AccountCode
            AccountName = dao.AccountName
            AccountNameKana = if isNull dao.AccountNameKana then None else Some dao.AccountNameKana
            AccountType = dao.AccountType
            IsSummaryAccount = dao.IsSummaryAccount
            BsplType = if isNull dao.BsplType then None else Some dao.BsplType
            TransactionElementType = if isNull dao.TransactionElementType then None else Some dao.TransactionElementType
            ExpenseType = if isNull dao.ExpenseType then None else Some dao.ExpenseType
            DisplayOrder = dao.DisplayOrder
            IsAggregationTarget = dao.IsAggregationTarget
            TaxCode = if isNull dao.TaxCode then None else Some dao.TaxCode
            Balance = dao.Balance
        }

    /// ドメインモデルから DAO へ変換（INSERT/UPDATE 用パラメータ）
    let fromDomain (model: Account) =
        {|
            AccountCode = model.AccountCode
            AccountName = model.AccountName
            AccountNameKana = model.AccountNameKana |> Option.toObj
            AccountType = model.AccountType
            IsSummaryAccount = model.IsSummaryAccount
            BsplType = model.BsplType |> Option.toObj
            TransactionElementType = model.TransactionElementType |> Option.toObj
            ExpenseType = model.ExpenseType |> Option.toObj
            DisplayOrder = model.DisplayOrder
            IsAggregationTarget = model.IsAggregationTarget
            TaxCode = model.TaxCode |> Option.toObj
            Balance = model.Balance
        |}
