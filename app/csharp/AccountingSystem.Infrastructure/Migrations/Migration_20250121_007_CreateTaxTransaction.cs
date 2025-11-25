using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations;

/// <summary>
/// 課税取引マスタの作成
/// </summary>
[Migration(20250121007)]
public class Migration_20250121_007_CreateTaxTransaction : Migration
{
    public override void Up()
    {
        // 課税取引マスタ
        Create.Table("課税取引マスタ")
            .WithColumn("課税取引コード").AsString(2).PrimaryKey()
            .WithColumn("課税取引名").AsString(20).NotNullable()
            .WithColumn("税率").AsDecimal(5, 3).NotNullable().WithDefaultValue(0.000m)
            .WithColumn("説明").AsString(200).Nullable()
            .WithColumn("有効フラグ").AsBoolean().NotNullable().WithDefaultValue(true)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // CHECK 制約
        Execute.Sql(@"
            ALTER TABLE ""課税取引マスタ""
                ADD CONSTRAINT check_tax_rate
                CHECK (""税率"" >= 0 AND ""税率"" <= 1);
        ");

        // コメント追加
        Execute.Sql(@"
            COMMENT ON TABLE ""課税取引マスタ"" IS '消費税の課税取引区分を管理するマスタテーブル';
            COMMENT ON COLUMN ""課税取引マスタ"".""課税取引コード"" IS '課税取引コード（01:課税、02:非課税、03:免税、04:不課税）';
            COMMENT ON COLUMN ""課税取引マスタ"".""課税取引名"" IS '課税取引名';
            COMMENT ON COLUMN ""課税取引マスタ"".""税率"" IS '適用される税率（0.10 = 10%）';
            COMMENT ON COLUMN ""課税取引マスタ"".""説明"" IS '課税取引の説明';
            COMMENT ON COLUMN ""課税取引マスタ"".""有効フラグ"" IS '有効な課税取引区分かどうか';
        ");

        // 初期データ投入
        Insert.IntoTable("課税取引マスタ")
            .Row(new
            {
                課税取引コード = "01",
                課税取引名 = "課税",
                税率 = 0.10m,
                説明 = "消費税が課税される取引"
            })
            .Row(new
            {
                課税取引コード = "02",
                課税取引名 = "非課税",
                税率 = 0.000m,
                説明 = "消費税が非課税の取引（土地の譲渡、住宅の貸付など）"
            })
            .Row(new
            {
                課税取引コード = "03",
                課税取引名 = "免税",
                税率 = 0.000m,
                説明 = "消費税が免税の取引（輸出取引など）"
            })
            .Row(new
            {
                課税取引コード = "04",
                課税取引名 = "不課税",
                税率 = 0.000m,
                説明 = "消費税の対象外の取引（給与、配当など）"
            });

        // 勘定科目マスタに外部キー制約を追加
        Create.ForeignKey("fk_account_tax_code")
            .FromTable("勘定科目マスタ").ForeignColumn("課税取引コード")
            .ToTable("課税取引マスタ").PrimaryColumn("課税取引コード")
            .OnDelete(System.Data.Rule.SetNull);
    }

    public override void Down()
    {
        Delete.ForeignKey("fk_account_tax_code").OnTable("勘定科目マスタ");
        Delete.Table("課税取引マスタ");
    }
}
