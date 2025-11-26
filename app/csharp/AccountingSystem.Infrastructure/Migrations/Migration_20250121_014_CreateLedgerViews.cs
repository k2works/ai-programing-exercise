using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 総勘定元帳と試算表のビュー作成
/// </summary>
[Migration(20250121014)]
public class Migration_20250121_014_CreateLedgerViews : Migration
{
    public override void Up()
    {
        // 総勘定元帳ビュー
        Execute.Sql(@"
            CREATE OR REPLACE VIEW ""総勘定元帳"" AS
            SELECT
                d.""起票日"" as entry_date,
                a.""勘定科目コード"" as account_code,
                a.""勘定科目名"" as account_name,
                a.""BSPL区分"" as bspl_type,
                d.""補助科目コード"" as sub_account_code,
                d.""部門コード"" as department_code,
                d.""プロジェクトコード"" as project_code,
                d.""借方金額"" as debit_amount,
                d.""貸方金額"" as credit_amount,
                -- ウィンドウ関数で累積残高を計算
                SUM(d.""借方金額"" - d.""貸方金額"") OVER (
                    PARTITION BY d.""勘定科目コード"", d.""補助科目コード"", d.""部門コード"", d.""プロジェクトコード""
                    ORDER BY d.""起票日""
                    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
                ) as balance
            FROM ""日次勘定科目残高"" d
            INNER JOIN ""勘定科目マスタ"" a ON d.""勘定科目コード"" = a.""勘定科目コード""
            WHERE d.""決算仕訳フラグ"" = 0  -- 通常仕訳のみ
            ORDER BY d.""勘定科目コード"", d.""起票日"";

            COMMENT ON VIEW ""総勘定元帳"" IS '総勘定元帳（日次残高から生成される勘定科目ごとの取引履歴）';
        ");

        // 試算表ビュー
        Execute.Sql(@"
            CREATE OR REPLACE VIEW ""試算表"" AS
            SELECT
                a.""勘定科目コード"" as account_code,
                a.""勘定科目名"" as account_name,
                a.""BSPL区分"" as bspl_type,
                a.""取引要素区分"" as transaction_element_type,
                COALESCE(SUM(d.""借方金額""), 0) as debit_total,
                COALESCE(SUM(d.""貸方金額""), 0) as credit_total,
                -- 取引要素区分に応じて残高を計算（1:資産, 5:費用 は借方、2:負債, 3:純資産, 4:収益 は貸方）
                CASE
                    WHEN a.""取引要素区分"" IN ('1', '5') THEN
                        COALESCE(SUM(d.""借方金額""), 0) - COALESCE(SUM(d.""貸方金額""), 0)
                    ELSE
                        COALESCE(SUM(d.""貸方金額""), 0) - COALESCE(SUM(d.""借方金額""), 0)
                END as balance
            FROM ""勘定科目マスタ"" a
            LEFT JOIN ""日次勘定科目残高"" d
                ON a.""勘定科目コード"" = d.""勘定科目コード""
                AND d.""決算仕訳フラグ"" = 0  -- 通常仕訳のみ
            GROUP BY a.""勘定科目コード"", a.""勘定科目名"", a.""BSPL区分"", a.""取引要素区分""
            ORDER BY a.""勘定科目コード"";

            COMMENT ON VIEW ""試算表"" IS '試算表（全勘定科目の残高一覧）';
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"DROP VIEW IF EXISTS ""試算表""");
        Execute.Sql(@"DROP VIEW IF EXISTS ""総勘定元帳""");
    }
}
