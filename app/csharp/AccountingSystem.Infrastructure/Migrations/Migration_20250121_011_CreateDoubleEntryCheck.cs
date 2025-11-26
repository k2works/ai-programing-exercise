using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 複式簿記の原理をデータベースで保証するためのビューと関数を作成
///
/// - 仕訳残高チェックビュー: 仕訳ごとの借方・貸方の合計を算出
/// - 複式簿記チェック関数: 借方・貸方が一致しない仕訳を検出
/// </summary>
[Migration(20250121011)]
public class Migration_20250121_011_CreateDoubleEntryCheck : Migration
{
    public override void Up()
    {
        // 1. 仕訳残高チェックビュー
        Execute.Sql(@"
            CREATE OR REPLACE VIEW ""仕訳残高チェック"" AS
            SELECT
                ""仕訳伝票番号"",
                SUM(CASE WHEN ""仕訳行貸借区分"" = 'D' THEN ""仕訳金額"" ELSE 0 END) AS ""借方合計"",
                SUM(CASE WHEN ""仕訳行貸借区分"" = 'C' THEN ""仕訳金額"" ELSE 0 END) AS ""貸方合計"",
                SUM(CASE WHEN ""仕訳行貸借区分"" = 'D' THEN ""仕訳金額"" ELSE 0 END) -
                SUM(CASE WHEN ""仕訳行貸借区分"" = 'C' THEN ""仕訳金額"" ELSE 0 END) AS ""差額""
            FROM ""仕訳貸借明細""
            GROUP BY ""仕訳伝票番号""
        ");

        Execute.Sql(@"COMMENT ON VIEW ""仕訳残高チェック"" IS '仕訳ごとの借方・貸方の合計を算出するビュー'");

        // 2. 複式簿記チェック関数（差額が0でない仕訳を検出）
        Execute.Sql(@"
            CREATE OR REPLACE FUNCTION ""複式簿記チェック""()
            RETURNS TABLE(""不整合伝票番号"" VARCHAR(20), ""差額"" DECIMAL) AS $$
            BEGIN
                RETURN QUERY
                SELECT ""仕訳伝票番号"", (""借方合計"" - ""貸方合計"") as ""差額""
                FROM ""仕訳残高チェック""
                WHERE ""借方合計"" != ""貸方合計"";
            END;
            $$ LANGUAGE plpgsql
        ");

        Execute.Sql(@"COMMENT ON FUNCTION ""複式簿記チェック""() IS '借方・貸方が一致しない仕訳を検出する関数'");
    }

    public override void Down()
    {
        Execute.Sql(@"DROP FUNCTION IF EXISTS ""複式簿記チェック""()");
        Execute.Sql(@"DROP VIEW IF EXISTS ""仕訳残高チェック""");
    }
}
