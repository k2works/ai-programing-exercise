using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 在庫情報テーブルの作成
/// </summary>
[Migration(17)]
public class V017_CreateInventoryTables : Migration
{
    public override void Up()
    {
        // 在庫情報
        Create.Table("在庫情報")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("場所コード").AsString(20).NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("在庫数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("合格数").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("不良数").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("未検査数").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // ユニーク制約
        Create.UniqueConstraint("uq_在庫_場所_品目")
            .OnTable("在庫情報")
            .Columns("場所コード", "品目コード");

        // 外部キー
        Create.ForeignKey("fk_在庫_場所")
            .FromTable("在庫情報").ForeignColumn("場所コード")
            .ToTable("場所マスタ").PrimaryColumn("場所コード");

        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除

        // インデックス
        Create.Index("idx_在庫_場所").OnTable("在庫情報").OnColumn("場所コード");
        Create.Index("idx_在庫_品目").OnTable("在庫情報").OnColumn("品目コード");
    }

    public override void Down()
    {
        Delete.Table("在庫情報");
    }
}
