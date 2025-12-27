using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// ロットテーブルの作成
/// </summary>
[Migration(2002)]
public class V020_2_CreateLotTables : Migration
{
    public override void Up()
    {
        // ロット種別 ENUM
        Execute.Sql("CREATE TYPE \"ロット種別\" AS ENUM ('購入ロット', '製造ロット')");

        // ロットマスタ
        Create.Table("ロットマスタ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("ロット番号").AsString(30).NotNullable().Unique()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("ロット種別").AsCustom("\"ロット種別\"").NotNullable()
            .WithColumn("製造日").AsDate().Nullable()
            .WithColumn("有効期限").AsDate().Nullable()
            .WithColumn("数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // ロット構成
        Create.Table("ロット構成")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("親ロット番号").AsString(30).NotNullable()
            .WithColumn("子ロット番号").AsString(30).NotNullable()
            .WithColumn("使用数量").AsDecimal(15, 2).NotNullable();

        // 外部キー
        Create.ForeignKey("fk_ロット構成_親")
            .FromTable("ロット構成").ForeignColumn("親ロット番号")
            .ToTable("ロットマスタ").PrimaryColumn("ロット番号");

        Create.ForeignKey("fk_ロット構成_子")
            .FromTable("ロット構成").ForeignColumn("子ロット番号")
            .ToTable("ロットマスタ").PrimaryColumn("ロット番号");

        // ユニーク制約
        Create.UniqueConstraint("uq_ロット構成")
            .OnTable("ロット構成").Columns("親ロット番号", "子ロット番号");

        // インデックス
        Create.Index("idx_ロット_品目").OnTable("ロットマスタ").OnColumn("品目コード");
        Create.Index("idx_ロット構成_親").OnTable("ロット構成").OnColumn("親ロット番号");
        Create.Index("idx_ロット構成_子").OnTable("ロット構成").OnColumn("子ロット番号");
    }

    public override void Down()
    {
        Delete.Table("ロット構成");
        Delete.Table("ロットマスタ");
        Execute.Sql("DROP TYPE \"ロット種別\"");
    }
}
