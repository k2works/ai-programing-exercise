namespace AccountingSystem.Application.Services

open System
open System.Threading.Tasks
open AccountingSystem.Application.Repositories

/// <summary>
/// 残高管理アプリケーションサービス
/// 仕訳入力時に日次残高を即時更新するためのサービス
/// Infrastructure への依存は IBalanceRepository を通じて抽象化
/// </summary>
type BalanceService(repository: IBalanceRepository) =

    /// <summary>
    /// 日次残高を更新（UPSERT）
    /// PostgreSQL の ON CONFLICT ... DO UPDATE を使用して、
    /// 同じキーの残高がある場合は金額を加算する
    /// </summary>
    /// <param name="entryDate">起票日</param>
    /// <param name="accountCode">勘定科目コード</param>
    /// <param name="subAccountCode">補助科目コード</param>
    /// <param name="departmentCode">部門コード</param>
    /// <param name="projectCode">プロジェクトコード</param>
    /// <param name="settlementFlag">決算仕訳フラグ</param>
    /// <param name="debitAmount">借方金額</param>
    /// <param name="creditAmount">貸方金額</param>
    member this.UpdateDailyBalanceAsync(
        entryDate: DateTime,
        accountCode: string,
        subAccountCode: string option,
        departmentCode: string option,
        projectCode: string option,
        settlementFlag: int option,
        debitAmount: decimal,
        creditAmount: decimal) : Task =
        task {
            let param: DailyBalanceUpdateParams = {
                EntryDate = entryDate
                AccountCode = accountCode
                SubAccountCode = subAccountCode
                DepartmentCode = departmentCode
                ProjectCode = projectCode
                SettlementFlag = settlementFlag
                DebitAmount = debitAmount
                CreditAmount = creditAmount
            }
            do! repository.UpdateDailyBalanceAsync(param)
        }

    /// <summary>
    /// 仕訳貸借明細から日次残高を一括更新
    /// 指定された仕訳番号の明細を集計して残高テーブルを更新
    /// </summary>
    member this.UpdateBalanceFromJournalItemsAsync(journalNo: string) : Task =
        task {
            do! repository.UpdateBalanceFromJournalItemsAsync(journalNo)
        }

    /// <summary>
    /// 指定期間の日次残高を月次残高に集計
    /// </summary>
    member this.ConsolidateMonthlyBalanceAsync(fiscalYear: int, month: int) : Task =
        task {
            let param: MonthlyBalanceConsolidateParams = {
                FiscalYear = fiscalYear
                Month = month
            }
            do! repository.ConsolidateMonthlyBalanceAsync(param)
        }
