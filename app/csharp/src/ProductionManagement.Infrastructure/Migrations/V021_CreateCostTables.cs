using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 原価管理テーブルの作成
/// </summary>
[Migration(21)]
public class V021_CreateCostTables : Migration
{
    public override void Up()
    {
        // 標準原価マスタ
        Create.Table("標準原価マスタ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("適用開始日").AsDate().NotNullable()
            .WithColumn("適用終了日").AsDate().Nullable()
            .WithColumn("標準材料費").AsDecimal(15, 2).NotNullable()
            .WithColumn("標準労務費").AsDecimal(15, 2).NotNullable()
            .WithColumn("標準経費").AsDecimal(15, 2).NotNullable()
            .WithColumn("標準製造原価").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 実際原価データ
        Create.Table("実際原価データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("作業指示番号").AsString(20).NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("完成数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("実際材料費").AsDecimal(15, 2).NotNullable()
            .WithColumn("実際労務費").AsDecimal(15, 2).NotNullable()
            .WithColumn("実際経費").AsDecimal(15, 2).NotNullable()
            .WithColumn("実際製造原価").AsDecimal(15, 2).NotNullable()
            .WithColumn("単位原価").AsDecimal(15, 4).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 原価差異データ
        Create.Table("原価差異データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("作業指示番号").AsString(20).NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("材料費差異").AsDecimal(15, 2).NotNullable()
            .WithColumn("労務費差異").AsDecimal(15, 2).NotNullable()
            .WithColumn("経費差異").AsDecimal(15, 2).NotNullable()
            .WithColumn("総差異").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // ユニーク制約
        Create.UniqueConstraint("uq_標準原価_品目_適用開始日")
            .OnTable("標準原価マスタ").Columns("品目コード", "適用開始日");

        Create.UniqueConstraint("uq_実際原価_作業指示番号")
            .OnTable("実際原価データ").Column("作業指示番号");

        // 外部キー
        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のためアプリケーション層でバリデーション

        Create.ForeignKey("fk_実際原価_作業指示")
            .FromTable("実際原価データ").ForeignColumn("作業指示番号")
            .ToTable("作業指示データ").PrimaryColumn("作業指示番号");

        Create.ForeignKey("fk_原価差異_作業指示")
            .FromTable("原価差異データ").ForeignColumn("作業指示番号")
            .ToTable("作業指示データ").PrimaryColumn("作業指示番号");

        // インデックス
        Create.Index("idx_標準原価_品目").OnTable("標準原価マスタ").OnColumn("品目コード");
        Create.Index("idx_実際原価_作業指示").OnTable("実際原価データ").OnColumn("作業指示番号");
        Create.Index("idx_原価差異_作業指示").OnTable("原価差異データ").OnColumn("作業指示番号");
    }

    public override void Down()
    {
        Delete.Table("原価差異データ");
        Delete.Table("実際原価データ");
        Delete.Table("標準原価マスタ");
    }
}
