namespace ManagementAccounting.Infrastructure.Adapters.External

open System
open System.Net.Http
open System.Net.Http.Json
open System.Threading.Tasks
open ManagementAccounting.Domain.Models
open ManagementAccounting.Application.Ports.Out

/// <summary>
/// 財務会計サービスから取得する仕訳 DTO
/// </summary>
[<CLIMutable>]
type JournalDto = {
    JournalId: int
    JournalDate: DateTime
    Description: string
    FiscalYear: int
}

/// <summary>
/// 財務会計サービスから取得する仕訳明細 DTO
/// </summary>
[<CLIMutable>]
type JournalEntryDto = {
    AccountCode: string
    DebitAmount: decimal
    CreditAmount: decimal
    Description: string
}

/// <summary>
/// 財務会計サービスから取得する勘定科目 DTO
/// </summary>
[<CLIMutable>]
type AccountDto = {
    AccountId: int
    AccountCode: string
    AccountName: string
    AccountType: string
    Balance: decimal
}

/// <summary>
/// 財務会計サービスクライアント（腐敗防止層 - ACL）
/// </summary>
type FinancialAccountingClient(httpClient: HttpClient) =

    let convertToFinancialData (fiscalYear: int) (accounts: AccountDto list) : FinancialData =
        // 勘定科目種別ごとに残高を集計
        let getBalanceByType accountType =
            accounts
            |> List.filter (fun a -> a.AccountType = accountType)
            |> List.sumBy (fun a -> a.Balance)

        let getBalanceByCode code =
            accounts
            |> List.tryFind (fun a -> a.AccountCode = code)
            |> Option.map (fun a -> a.Balance)
            |> Option.defaultValue 0m

        // 資産（流動資産 + 固定資産）
        let assets = getBalanceByType "Asset"

        // 負債
        let liabilities = getBalanceByType "Liability"

        // 純資産
        let equity = getBalanceByType "Equity"

        // 収益（売上高）
        let revenue = getBalanceByType "Revenue"

        // 費用
        let expenses = getBalanceByType "Expense"

        // 営業利益 = 売上高 - 費用（簡易計算）
        let operatingProfit = revenue - expenses

        // 流動資産（勘定科目コード 1xxx）
        let currentAssets =
            accounts
            |> List.filter (fun a -> a.AccountType = "Asset" && a.AccountCode.StartsWith("1"))
            |> List.sumBy (fun a -> a.Balance)

        // 固定資産（勘定科目コード 2xxx）
        let fixedAssets =
            accounts
            |> List.filter (fun a -> a.AccountType = "Asset" && a.AccountCode.StartsWith("2"))
            |> List.sumBy (fun a -> a.Balance)

        // 流動負債
        let currentLiabilities =
            accounts
            |> List.filter (fun a -> a.AccountType = "Liability" && a.AccountCode.StartsWith("3"))
            |> List.sumBy (fun a -> a.Balance)

        // 当座資産（現金預金 + 売掛金）
        let quickAssets =
            accounts
            |> List.filter (fun a ->
                a.AccountCode = "1110" || // 現金預金
                a.AccountCode = "1120")   // 売掛金
            |> List.sumBy (fun a -> a.Balance)

        {
            FiscalYear = fiscalYear
            Sales = revenue
            OperatingProfit = operatingProfit
            TotalAssets = assets
            TangibleFixedAssets = fixedAssets
            CurrentAssets = currentAssets
            CurrentLiabilities = if currentLiabilities = 0m then liabilities else currentLiabilities
            QuickAssets = quickAssets
            Equity = equity
        }

    interface IFinancialDataPort with
        member _.FetchFinancialDataByFiscalYearAsync(fiscalYear: int) : Task<FinancialData> =
            task {
                // 財務会計サービスから勘定科目一覧を取得
                let! response = httpClient.GetAsync("/api/accounts")
                response.EnsureSuccessStatusCode() |> ignore

                let! accounts = response.Content.ReadFromJsonAsync<AccountDto list>()
                let accountList = accounts |> Option.ofObj |> Option.defaultValue []

                return convertToFinancialData fiscalYear accountList
            }
