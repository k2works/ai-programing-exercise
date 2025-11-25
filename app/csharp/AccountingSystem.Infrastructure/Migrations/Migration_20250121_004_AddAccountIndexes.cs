using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 勘定科目マスタにインデックスを追加
/// </summary>
[Migration(20250121004)]
public class Migration_20250121_004_AddAccountIndexes : Migration
{
    public override void Up()
    {
        // BSPL区分のインデックス
        Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_bspl_distinction
                ON ""勘定科目マスタ"" (""BSPL区分"")
                WHERE ""BSPL区分"" IS NOT NULL;
        ");

        // 取引要素区分のインデックス
        Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_transaction_distinction
                ON ""勘定科目マスタ"" (""取引要素区分"")
                WHERE ""取引要素区分"" IS NOT NULL;
        ");

        // 勘定科目種別のインデックス
        Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_type
                ON ""勘定科目マスタ"" (""勘定科目種別"");
        ");

        // 表示順序のインデックス
        Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_display_order
                ON ""勘定科目マスタ"" (""表示順序"")
                WHERE ""表示順序"" IS NOT NULL;
        ");

        // 複合インデックス（BSPL区分 + 表示順序）
        Execute.Sql(@"
            CREATE INDEX IF NOT EXISTS idx_account_bspl_display_order
                ON ""勘定科目マスタ"" (""BSPL区分"", ""表示順序"")
                WHERE ""BSPL区分"" IS NOT NULL AND ""表示順序"" IS NOT NULL;
        ");

        // コメント追加
        Execute.Sql(@"
            COMMENT ON INDEX idx_account_bspl_distinction IS
                'BSPL区分での検索を高速化';

            COMMENT ON INDEX idx_account_transaction_distinction IS
                '取引要素区分での検索を高速化';

            COMMENT ON INDEX idx_account_type IS
                '勘定科目種別での検索を高速化';

            COMMENT ON INDEX idx_account_display_order IS
                '表示順序でのソートを高速化';

            COMMENT ON INDEX idx_account_bspl_display_order IS
                'BSPL区分での絞り込み + 表示順序でのソートを高速化（財務諸表表示用）';
        ");
    }

    public override void Down()
    {
        Execute.Sql(@"
            DROP INDEX IF EXISTS idx_account_bspl_distinction;
            DROP INDEX IF EXISTS idx_account_transaction_distinction;
            DROP INDEX IF EXISTS idx_account_type;
            DROP INDEX IF EXISTS idx_account_display_order;
            DROP INDEX IF EXISTS idx_account_bspl_display_order;
        ");
    }
}
