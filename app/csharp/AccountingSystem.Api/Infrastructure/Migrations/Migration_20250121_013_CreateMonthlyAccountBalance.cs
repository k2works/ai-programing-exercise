using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 月次勘定科目残高テーブルの作成
/// </summary>
[Migration(20250121013)]
public class Migration_20250121_013_CreateMonthlyAccountBalance : Migration
{
    public override void Up()
    {
        Create.Table("月次勘定科目残高")
            .WithColumn("決算期").AsInt32().NotNullable()
            .WithColumn("月度").AsInt32().NotNullable()
            .WithColumn("勘定科目コード").AsString(20).NotNullable()
            .WithColumn("補助科目コード").AsString(10).NotNullable().WithDefaultValue("")
            .WithColumn("部門コード").AsString(5).NotNullable().WithDefaultValue("")
            .WithColumn("プロジェクトコード").AsString(10).NotNullable().WithDefaultValue("")
            .WithColumn("決算仕訳フラグ").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("月初残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("借方金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("貸方金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("月末残高").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 複合主キー
        Create.PrimaryKey("PK_月次勘定科目残高")
            .OnTable("月次勘定科目残高")
            .Columns("決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ");

        // 外部キー制約
        Create.ForeignKey("FK_月次勘定科目残高_勘定科目マスタ")
            .FromTable("月次勘定科目残高").ForeignColumn("勘定科目コード")
            .ToTable("勘定科目マスタ").PrimaryColumn("勘定科目コード");

        // インデックス作成
        Create.Index("idx_月次勘定科目残高_決算期月度")
            .OnTable("月次勘定科目残高")
            .OnColumn("決算期").Ascending()
            .OnColumn("月度").Ascending();

        Create.Index("idx_月次勘定科目残高_勘定科目")
            .OnTable("月次勘定科目残高")
            .OnColumn("勘定科目コード");

        // CHECK 制約
        Execute.Sql(@"
            ALTER TABLE ""月次勘定科目残高""
              ADD CONSTRAINT ""check_月次残高_月度範囲""
              CHECK (""月度"" >= 1 AND ""月度"" <= 12)
        ");

        // コメント追加
        Execute.Sql(@"
            COMMENT ON TABLE ""月次勘定科目残高"" IS '月次勘定科目残高（月ごとの月初残高・借方・貸方金額・月末残高を記録）';
            COMMENT ON COLUMN ""月次勘定科目残高"".""決算期"" IS '会計年度（例：20250000）';
            COMMENT ON COLUMN ""月次勘定科目残高"".""月度"" IS '月度（1～12）';
            COMMENT ON COLUMN ""月次勘定科目残高"".""月初残高"" IS '月初時点の残高';
            COMMENT ON COLUMN ""月次勘定科目残高"".""月末残高"" IS '月末時点の残高（月初残高 + 借方金額 - 貸方金額）';
        ");
    }

    public override void Down()
    {
        Delete.Table("月次勘定科目残高");
    }
}
