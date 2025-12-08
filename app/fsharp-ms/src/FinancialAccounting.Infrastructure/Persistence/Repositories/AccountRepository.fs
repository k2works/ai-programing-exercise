namespace FinancialAccounting.Infrastructure.Persistence.Repositories

open System
open System.Data
open System.Threading.Tasks
open Npgsql
open Dapper
open FinancialAccounting.Domain.Entities
open FinancialAccounting.Application.Ports.Out

/// <summary>
/// DB 行モデル（accounts テーブル）
/// </summary>
[<CLIMutable>]
type AccountRow = {
    account_id: int
    account_code: string
    account_name: string
    account_name_kana: string
    account_type: string
    is_summary_account: bool
    bs_pl_type: string
    transaction_element_type: string
    expense_type: string
    display_order: int
    is_aggregation_target: bool
    tax_code: string
    balance: decimal
    created_at: DateTime
    updated_at: DateTime
}

/// <summary>
/// 勘定科目リポジトリ実装（Dapper）
/// </summary>
type AccountRepository(connectionString: string) =

    let createConnection () : IDbConnection =
        new NpgsqlConnection(connectionString)

    let toAccount (row: AccountRow) : Account =
        let accountType =
            Account.stringToAccountType row.account_type
            |> Option.defaultValue Asset

        let bsPlType =
            Account.stringToBsPlType row.bs_pl_type
            |> Option.defaultValue BalanceSheet

        let transactionElementType =
            Account.stringToTransactionElementType row.transaction_element_type
            |> Option.defaultValue Debit

        {
            AccountId = Some row.account_id
            AccountCode = row.account_code
            AccountName = row.account_name
            AccountNameKana = row.account_name_kana |> Option.ofObj
            AccountType = accountType
            IsSummaryAccount = row.is_summary_account
            BsPlType = bsPlType
            TransactionElementType = transactionElementType
            ExpenseType = row.expense_type |> Option.ofObj
            DisplayOrder = row.display_order
            IsAggregationTarget = row.is_aggregation_target
            TaxCode = row.tax_code |> Option.ofObj
            Balance = row.balance
            CreatedAt = row.created_at
            UpdatedAt = row.updated_at
        }

    interface IAccountRepository with
        member _.SaveAsync(account: Account) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = """
                    INSERT INTO accounts (
                        account_code, account_name, account_name_kana, account_type,
                        is_summary_account, bs_pl_type, transaction_element_type,
                        expense_type, display_order, is_aggregation_target, tax_code, balance
                    ) VALUES (
                        @AccountCode, @AccountName, @AccountNameKana, @AccountType,
                        @IsSummaryAccount, @BsPlType, @TransactionElementType,
                        @ExpenseType, @DisplayOrder, @IsAggregationTarget, @TaxCode, @Balance
                    ) RETURNING account_id
                """

                let parameters = {|
                    AccountCode = account.AccountCode
                    AccountName = account.AccountName
                    AccountNameKana = account.AccountNameKana |> Option.toObj
                    AccountType = Account.accountTypeToString account.AccountType
                    IsSummaryAccount = account.IsSummaryAccount
                    BsPlType = Account.bsPlTypeToString account.BsPlType
                    TransactionElementType = Account.transactionElementTypeToString account.TransactionElementType
                    ExpenseType = account.ExpenseType |> Option.toObj
                    DisplayOrder = account.DisplayOrder
                    IsAggregationTarget = account.IsAggregationTarget
                    TaxCode = account.TaxCode |> Option.toObj
                    Balance = account.Balance
                |}

                let! accountId = connection.ExecuteScalarAsync<int>(sql, parameters)
                return { account with AccountId = Some accountId }
            }

        member _.GetByIdAsync(id: int) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "SELECT * FROM accounts WHERE account_id = @Id"
                let! results = connection.QueryAsync<AccountRow>(sql, {| Id = id |})

                return
                    results
                    |> Seq.tryHead
                    |> Option.map toAccount
            }

        member _.GetByCodeAsync(code: string) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "SELECT * FROM accounts WHERE account_code = @Code"
                let! results = connection.QueryAsync<AccountRow>(sql, {| Code = code |})

                return
                    results
                    |> Seq.tryHead
                    |> Option.map toAccount
            }

        member _.GetAllAsync() =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "SELECT * FROM accounts ORDER BY display_order, account_code"
                let! results = connection.QueryAsync<AccountRow>(sql)

                return
                    results
                    |> Seq.map toAccount
                    |> Seq.toList
            }

        member _.GetByTypeAsync(accountType: AccountType) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "SELECT * FROM accounts WHERE account_type = @AccountType ORDER BY display_order, account_code"
                let! results = connection.QueryAsync<AccountRow>(sql, {| AccountType = Account.accountTypeToString accountType |})

                return
                    results
                    |> Seq.map toAccount
                    |> Seq.toList
            }

        member _.UpdateAsync(account: Account) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = """
                    UPDATE accounts SET
                        account_name = @AccountName,
                        account_name_kana = @AccountNameKana,
                        is_summary_account = @IsSummaryAccount,
                        expense_type = @ExpenseType,
                        display_order = @DisplayOrder,
                        is_aggregation_target = @IsAggregationTarget,
                        tax_code = @TaxCode,
                        updated_at = @UpdatedAt
                    WHERE account_id = @AccountId
                """

                let parameters = {|
                    AccountId = account.AccountId |> Option.defaultValue 0
                    AccountName = account.AccountName
                    AccountNameKana = account.AccountNameKana |> Option.toObj
                    IsSummaryAccount = account.IsSummaryAccount
                    ExpenseType = account.ExpenseType |> Option.toObj
                    DisplayOrder = account.DisplayOrder
                    IsAggregationTarget = account.IsAggregationTarget
                    TaxCode = account.TaxCode |> Option.toObj
                    UpdatedAt = DateTime.UtcNow
                |}

                let! _ = connection.ExecuteAsync(sql, parameters)
                return account
            }

        member _.DeleteAsync(id: int) =
            task {
                use connection = createConnection()
                connection.Open()

                let sql = "DELETE FROM accounts WHERE account_id = @Id"
                let! rowsAffected = connection.ExecuteAsync(sql, {| Id = id |})
                return rowsAffected > 0
            }
