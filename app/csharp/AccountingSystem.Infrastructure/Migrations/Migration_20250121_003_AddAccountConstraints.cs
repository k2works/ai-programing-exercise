using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 勘定科目マスタに制約を追加
/// </summary>
[Migration(20250121003)]
public class Migration_20250121_003_AddAccountConstraints : Migration
{
    public override void Up()
    {
        // BSPL区分のCHECK制約
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ""
                ADD CONSTRAINT check_bspl_distinction
                CHECK (""BSPL区分"" IN ('B', 'P') OR ""BSPL区分"" IS NULL);
        ");

        // 取引要素区分のCHECK制約
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ""
                ADD CONSTRAINT check_transaction_distinction
                CHECK (""取引要素区分"" IN ('1', '2', '3', '4', '5') OR ""取引要素区分"" IS NULL);
        ");

        // 費用区分のCHECK制約
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ""
                ADD CONSTRAINT check_cost_distinction
                CHECK (""費用区分"" IN ('1', '2', '3') OR ""費用区分"" IS NULL);
        ");

        // BSPL区分と勘定科目種別の整合性チェック
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ""
                ADD CONSTRAINT check_bspl_consistency
                CHECK (
                    (""BSPL区分"" = 'B' AND ""勘定科目種別""::text IN ('資産', '負債', '純資産'))
                    OR
                    (""BSPL区分"" = 'P' AND ""勘定科目種別""::text IN ('収益', '費用'))
                    OR
                    (""BSPL区分"" IS NULL)
                );
        ");

        // 取引要素区分と勘定科目種別の整合性チェック
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ""
                ADD CONSTRAINT check_transaction_consistency
                CHECK (
                    (""取引要素区分"" = '1' AND ""勘定科目種別""::text = '資産')
                    OR
                    (""取引要素区分"" = '2' AND ""勘定科目種別""::text = '負債')
                    OR
                    (""取引要素区分"" = '3' AND ""勘定科目種別""::text = '純資産')
                    OR
                    (""取引要素区分"" = '4' AND ""勘定科目種別""::text = '収益')
                    OR
                    (""取引要素区分"" = '5' AND ""勘定科目種別""::text = '費用')
                    OR
                    (""取引要素区分"" IS NULL)
                );
        ");

        // 費用区分は費用科目のみ設定可能
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ""
                ADD CONSTRAINT check_cost_distinction_only_expense
                CHECK (
                    (""費用区分"" IS NOT NULL AND ""勘定科目種別""::text = '費用')
                    OR
                    (""費用区分"" IS NULL)
                );
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"
            ALTER TABLE ""勘定科目マスタ"" DROP CONSTRAINT IF EXISTS check_bspl_distinction;
            ALTER TABLE ""勘定科目マスタ"" DROP CONSTRAINT IF EXISTS check_transaction_distinction;
            ALTER TABLE ""勘定科目マスタ"" DROP CONSTRAINT IF EXISTS check_cost_distinction;
            ALTER TABLE ""勘定科目マスタ"" DROP CONSTRAINT IF EXISTS check_bspl_consistency;
            ALTER TABLE ""勘定科目マスタ"" DROP CONSTRAINT IF EXISTS check_transaction_consistency;
            ALTER TABLE ""勘定科目マスタ"" DROP CONSTRAINT IF EXISTS check_cost_distinction_only_expense;
        ");
    }
}
