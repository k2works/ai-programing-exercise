using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 工程マスタ・工程表の作成
/// </summary>
[Migration(13)]
public class V013_CreateProcessMasterTables : Migration
{
    public override void Up()
    {
        // 工程マスタ
        Create.Table("工程マスタ")
            .WithColumn("工程コード").AsString(20).PrimaryKey()
            .WithColumn("工程名").AsString(100).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        // 工程表
        Create.Table("工程表")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("工順").AsInt32().NotNullable()
            .WithColumn("工程コード").AsString(20).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 外部キー（工程マスタへのみ、品目マスタは複合キーのため参照しない）
        Create.ForeignKey("fk_工程表_工程")
            .FromTable("工程表").ForeignColumn("工程コード")
            .ToTable("工程マスタ").PrimaryColumn("工程コード");

        // ユニーク制約
        Create.UniqueConstraint("uq_工程表_品目_工順")
            .OnTable("工程表")
            .Columns("品目コード", "工順");

        // インデックス
        Create.Index("idx_工程表_品目コード").OnTable("工程表").OnColumn("品目コード");
        Create.Index("idx_工程表_工程コード").OnTable("工程表").OnColumn("工程コード");
    }

    public override void Down()
    {
        Delete.Table("工程表");
        Delete.Table("工程マスタ");
    }
}
