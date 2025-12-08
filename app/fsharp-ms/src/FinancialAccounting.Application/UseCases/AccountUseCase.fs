namespace FinancialAccounting.Application.UseCases

open System
open System.Threading.Tasks
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Application.Ports.Out

/// <summary>
/// 勘定科目ユースケース実装
/// </summary>
type AccountUseCase(repository: IAccountRepository) =

    let toAccount (request: CreateAccountRequest) : Result<Account, string> =
        // AccountType の変換
        let accountTypeResult =
            match Account.stringToAccountType request.AccountType with
            | Some t -> Ok t
            | None -> Error $"無効な勘定科目種別: {request.AccountType}"

        // BsPlType の変換
        let bsPlTypeResult =
            match Account.stringToBsPlType request.BsPlType with
            | Some t -> Ok t
            | None -> Error $"無効なBS/PL区分: {request.BsPlType}"

        // TransactionElementType の変換
        let transactionElementTypeResult =
            match Account.stringToTransactionElementType request.TransactionElementType with
            | Some t -> Ok t
            | None -> Error $"無効な取引要素区分: {request.TransactionElementType}"

        match accountTypeResult, bsPlTypeResult, transactionElementTypeResult with
        | Ok accountType, Ok bsPlType, Ok transactionElementType ->
            match Account.create request.AccountCode request.AccountName accountType bsPlType transactionElementType with
            | Ok account ->
                Ok { account with
                        AccountNameKana = request.AccountNameKana
                        IsSummaryAccount = request.IsSummaryAccount
                        ExpenseType = request.ExpenseType
                        DisplayOrder = request.DisplayOrder
                        IsAggregationTarget = request.IsAggregationTarget
                        TaxCode = request.TaxCode }
            | Error msg -> Error msg
        | Error msg, _, _ -> Error msg
        | _, Error msg, _ -> Error msg
        | _, _, Error msg -> Error msg

    interface IAccountUseCase with
        member _.CreateAccountAsync(request: CreateAccountRequest) =
            task {
                match toAccount request with
                | Error msg -> return Error msg
                | Ok account ->
                    // 重複チェック
                    let! existing = repository.GetByCodeAsync(request.AccountCode)
                    match existing with
                    | Some _ -> return Error $"勘定科目コード {request.AccountCode} は既に存在します"
                    | None ->
                        let! saved = repository.SaveAsync(account)
                        return Ok saved
            }

        member _.GetAccountByIdAsync(id: int) =
            repository.GetByIdAsync(id)

        member _.GetAccountByCodeAsync(code: string) =
            repository.GetByCodeAsync(code)

        member _.GetAllAccountsAsync() =
            repository.GetAllAsync()

        member _.GetAccountsByTypeAsync(accountType: string) =
            task {
                match Account.stringToAccountType accountType with
                | Some t ->
                    let! accounts = repository.GetByTypeAsync(t)
                    return Ok accounts
                | None ->
                    return Error $"無効な勘定科目種別: {accountType}"
            }

        member _.UpdateAccountAsync(request: UpdateAccountRequest) =
            task {
                let! existing = repository.GetByIdAsync(request.AccountId)
                match existing with
                | None -> return Error $"勘定科目ID {request.AccountId} が見つかりません"
                | Some account ->
                    let updated = { account with
                                        AccountName = request.AccountName
                                        AccountNameKana = request.AccountNameKana
                                        IsSummaryAccount = request.IsSummaryAccount
                                        ExpenseType = request.ExpenseType
                                        DisplayOrder = request.DisplayOrder
                                        IsAggregationTarget = request.IsAggregationTarget
                                        TaxCode = request.TaxCode
                                        UpdatedAt = DateTime.UtcNow }
                    let! saved = repository.UpdateAsync(updated)
                    return Ok saved
            }

        member _.DeleteAccountAsync(id: int) =
            task {
                let! existing = repository.GetByIdAsync(id)
                match existing with
                | None -> return Error $"勘定科目ID {id} が見つかりません"
                | Some _ ->
                    let! result = repository.DeleteAsync(id)
                    return Ok result
            }
