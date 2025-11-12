using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations;

/// <summary>
/// 自動採番マスタのマイグレーション
/// </summary>
[Migration(20250106031)]
public class Migration20250106031CreateAutoNumberTable : Migration
{
    public override void Up()
    {
        Create.Table("自動採番マスタ")
            .WithColumn("伝票種別コード").AsString(2).NotNullable()
            .WithColumn("年月").AsString(6).NotNullable()
            .WithColumn("最終伝票番号").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable()
            .WithColumn("作成者名").AsString(100).NotNullable()
            .WithColumn("更新日時").AsDateTime().NotNullable()
            .WithColumn("更新者名").AsString(100).NotNullable();

        Create.PrimaryKey("pk_自動採番マスタ")
            .OnTable("自動採番マスタ")
            .Columns("伝票種別コード", "年月");

        IfDatabase("Postgres").Execute.Sql(@"
            COMMENT ON TABLE 自動採番マスタ IS '自動採番マスタ';
            COMMENT ON COLUMN 自動採番マスタ.伝票種別コード IS '伝票種別コード (OR:受注 SA:売上 PO:発注 PU:仕入 IN:請求 CR:入金 PA:支払)';
            COMMENT ON COLUMN 自動採番マスタ.年月 IS '年月 (YYYYMM形式)';
            COMMENT ON COLUMN 自動採番マスタ.最終伝票番号 IS '最終伝票番号（この年月で最後に発番した番号）';
        ");

        Create.Index("idx_自動採番マスタ_年月")
            .OnTable("自動採番マスタ")
            .OnColumn("年月");

        // 初期データの投入（よく使う伝票種別を登録）
        var currentYearMonth = DateTime.Now.ToString("yyyyMM");

        Execute.Sql($@"
            INSERT INTO 自動採番マスタ (伝票種別コード, 年月, 最終伝票番号, 作成日時, 作成者名, 更新日時, 更新者名)
            VALUES
                ('OR', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration'),
                ('SA', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration'),
                ('PO', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration'),
                ('PU', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration'),
                ('IN', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration'),
                ('CR', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration'),
                ('PA', '{currentYearMonth}', 0, CURRENT_TIMESTAMP, 'migration', CURRENT_TIMESTAMP, 'migration')
        ");
    }

    public override void Down()
    {
        Delete.Table("自動採番マスタ");
    }
}
