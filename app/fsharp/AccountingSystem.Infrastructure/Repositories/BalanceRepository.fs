namespace AccountingSystem.Infrastructure.Repositories

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open AccountingSystem.Application.Repositories

/// <summary>
/// 残高リポジトリの PostgreSQL 実装
/// IBalanceRepository インターフェースを実装
/// </summary>
type BalanceRepository(connection: NpgsqlConnection) =

    interface IBalanceRepository with
        /// <summary>
        /// 日次残高を更新（UPSERT）
        /// </summary>
        member this.UpdateDailyBalanceAsync(param: DailyBalanceUpdateParams) : Task<unit> =
            task {
                let sql = """
                    INSERT INTO "日次勘定科目残高" (
                        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                    ) VALUES (@EntryDate, @AccountCode, @SubAccountCode, @DepartmentCode, @ProjectCode, @SettlementFlag, @DebitAmount, @CreditAmount)
                    ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
                    DO UPDATE SET
                        "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
                        "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
                        "更新日時" = CURRENT_TIMESTAMP
                    """

                let! _ = connection.ExecuteAsync(
                    sql,
                    {|
                        EntryDate = param.EntryDate
                        AccountCode = param.AccountCode
                        SubAccountCode = param.SubAccountCode |> Option.defaultValue ""
                        DepartmentCode = param.DepartmentCode |> Option.defaultValue ""
                        ProjectCode = param.ProjectCode |> Option.defaultValue ""
                        SettlementFlag = param.SettlementFlag |> Option.defaultValue 0
                        DebitAmount = param.DebitAmount
                        CreditAmount = param.CreditAmount
                    |})
                ()
            }

        /// <summary>
        /// 仕訳貸借明細から日次残高を一括更新
        /// </summary>
        member this.UpdateBalanceFromJournalItemsAsync(journalNo: string) : Task<unit> =
            task {
                let sql = """
                    INSERT INTO "日次勘定科目残高" (
                        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
                    )
                    SELECT
                        j."起票日",
                        item."勘定科目コード",
                        COALESCE(item."補助科目コード", ''),
                        COALESCE(item."部門コード", ''),
                        COALESCE(item."プロジェクトコード", ''),
                        j."決算仕訳フラグ",
                        SUM(CASE WHEN item."仕訳行貸借区分" = 'D' THEN item."仕訳金額" ELSE 0 END),
                        SUM(CASE WHEN item."仕訳行貸借区分" = 'C' THEN item."仕訳金額" ELSE 0 END)
                    FROM "仕訳貸借明細" item
                    INNER JOIN "仕訳明細" detail ON item."仕訳伝票番号" = detail."仕訳伝票番号"
                        AND item."仕訳行番号" = detail."仕訳行番号"
                    INNER JOIN "仕訳" j ON detail."仕訳伝票番号" = j."仕訳伝票番号"
                    WHERE j."仕訳伝票番号" = @JournalNo
                    GROUP BY
                        j."起票日",
                        item."勘定科目コード",
                        COALESCE(item."補助科目コード", ''),
                        COALESCE(item."部門コード", ''),
                        COALESCE(item."プロジェクトコード", ''),
                        j."決算仕訳フラグ"
                    ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
                    DO UPDATE SET
                        "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
                        "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
                        "更新日時" = CURRENT_TIMESTAMP
                    """

                let! _ = connection.ExecuteAsync(sql, {| JournalNo = journalNo |})
                ()
            }

        /// <summary>
        /// 指定期間の日次残高を月次残高に集計
        /// </summary>
        member this.ConsolidateMonthlyBalanceAsync(param: MonthlyBalanceConsolidateParams) : Task<unit> =
            task {
                let startDate = DateTime(param.FiscalYear, param.Month, 1)
                let endDate = startDate.AddMonths(1).AddDays(-1.0)

                let sql = """
                    INSERT INTO "月次勘定科目残高" (
                        "決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード",
                        "プロジェクトコード", "決算仕訳フラグ", "月初残高", "借方金額", "貸方金額", "月末残高"
                    )
                    SELECT
                        @FiscalYear,
                        @Month,
                        "勘定科目コード",
                        "補助科目コード",
                        "部門コード",
                        "プロジェクトコード",
                        "決算仕訳フラグ",
                        0,
                        SUM("借方金額"),
                        SUM("貸方金額"),
                        SUM("借方金額") - SUM("貸方金額")
                    FROM "日次勘定科目残高"
                    WHERE "起票日" >= @StartDate AND "起票日" <= @EndDate
                    GROUP BY
                        "勘定科目コード",
                        "補助科目コード",
                        "部門コード",
                        "プロジェクトコード",
                        "決算仕訳フラグ"
                    ON CONFLICT ("決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
                    DO UPDATE SET
                        "借方金額" = EXCLUDED."借方金額",
                        "貸方金額" = EXCLUDED."貸方金額",
                        "月末残高" = EXCLUDED."月末残高",
                        "更新日時" = CURRENT_TIMESTAMP
                    """

                let! _ = connection.ExecuteAsync(
                    sql,
                    {|
                        FiscalYear = param.FiscalYear
                        Month = param.Month
                        StartDate = startDate
                        EndDate = endDate
                    |})
                ()
            }
