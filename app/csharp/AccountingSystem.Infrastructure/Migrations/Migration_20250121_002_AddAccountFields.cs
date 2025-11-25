using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations
{
    /// <summary>
    /// 勘定科目マスタに実務項目を追加
    /// </summary>
    [Migration(20250121002)]
    public class Migration_20250121_002_AddAccountFields : Migration
    {
        public override void Up()
        {
            // BSPL区分を追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("BSPL区分").AsFixedLengthString(1).Nullable();

            // 取引要素区分を追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("取引要素区分").AsFixedLengthString(1).Nullable();

            // 費用区分を追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("費用区分").AsFixedLengthString(1).Nullable();

            // 合計科目フラグを追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("合計科目").AsBoolean().NotNullable().WithDefaultValue(false);

            // 表示順序を追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("表示順序").AsInt32().Nullable();

            // 集計対象フラグを追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("集計対象").AsBoolean().NotNullable().WithDefaultValue(true);

            // 勘定科目カナを追加
            Alter.Table("勘定科目マスタ")
                .AddColumn("勘定科目カナ").AsString(40).Nullable();

            // コメント追加
            Execute.Sql(@"
                COMMENT ON COLUMN ""勘定科目マスタ"".""BSPL区分"" IS
                    'BSPL区分（B:貸借対照表, P:損益計算書）';

                COMMENT ON COLUMN ""勘定科目マスタ"".""取引要素区分"" IS
                    '取引要素区分（1:資産, 2:負債, 3:純資産, 4:収益, 5:費用）';

                COMMENT ON COLUMN ""勘定科目マスタ"".""費用区分"" IS
                    '費用区分（1:売上原価, 2:販売費及び一般管理費, 3:営業外費用）';

                COMMENT ON COLUMN ""勘定科目マスタ"".""合計科目"" IS
                    '合計科目（true: 集計科目, false: 明細科目）';

                COMMENT ON COLUMN ""勘定科目マスタ"".""表示順序"" IS
                    '表示順序（財務諸表での表示順）';

                COMMENT ON COLUMN ""勘定科目マスタ"".""集計対象"" IS
                    '集計対象（true: 集計対象, false: 集計対象外）';

                COMMENT ON COLUMN ""勘定科目マスタ"".""勘定科目カナ"" IS
                    '勘定科目カナ（検索用）';
            ");
        }

        public override void Down()
        {
            Delete.Column("BSPL区分").FromTable("勘定科目マスタ");
            Delete.Column("取引要素区分").FromTable("勘定科目マスタ");
            Delete.Column("費用区分").FromTable("勘定科目マスタ");
            Delete.Column("合計科目").FromTable("勘定科目マスタ");
            Delete.Column("表示順序").FromTable("勘定科目マスタ");
            Delete.Column("集計対象").FromTable("勘定科目マスタ");
            Delete.Column("勘定科目カナ").FromTable("勘定科目マスタ");
        }
    }
}
