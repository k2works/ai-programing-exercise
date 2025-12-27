using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 工数実績関連テーブルの作成（部門マスタ、担当者マスタ、工数実績データ）
/// </summary>
[Migration(16)]
public class V016_CreateLaborHoursTables : Migration
{
    public override void Up()
    {
        // 部門マスタ
        Create.Table("部門マスタ")
            .WithColumn("部門コード").AsString(20).PrimaryKey()
            .WithColumn("部門名").AsString(100).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 担当者マスタ
        Create.Table("担当者マスタ")
            .WithColumn("担当者コード").AsString(20).PrimaryKey()
            .WithColumn("担当者名").AsString(100).NotNullable()
            .WithColumn("部門コード").AsString(20).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.ForeignKey("fk_担当者_部門")
            .FromTable("担当者マスタ").ForeignColumn("部門コード")
            .ToTable("部門マスタ").PrimaryColumn("部門コード");

        // 工数実績データ
        Create.Table("工数実績データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("工数実績番号").AsString(20).NotNullable().Unique()
            .WithColumn("作業指示番号").AsString(20).NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("工順").AsInt32().NotNullable()
            .WithColumn("工程コード").AsString(20).NotNullable()
            .WithColumn("部門コード").AsString(20).NotNullable()
            .WithColumn("担当者コード").AsString(20).NotNullable()
            .WithColumn("作業日").AsDate().NotNullable()
            .WithColumn("工数").AsDecimal(10, 2).NotNullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        // 外部キー
        Create.ForeignKey("fk_工数実績_作業指示")
            .FromTable("工数実績データ").ForeignColumn("作業指示番号")
            .ToTable("作業指示データ").PrimaryColumn("作業指示番号");

        // 品目マスタへのFKは複合主キーのため省略

        Create.ForeignKey("fk_工数実績_工程")
            .FromTable("工数実績データ").ForeignColumn("工程コード")
            .ToTable("工程マスタ").PrimaryColumn("工程コード");

        Create.ForeignKey("fk_工数実績_部門")
            .FromTable("工数実績データ").ForeignColumn("部門コード")
            .ToTable("部門マスタ").PrimaryColumn("部門コード");

        Create.ForeignKey("fk_工数実績_担当者")
            .FromTable("工数実績データ").ForeignColumn("担当者コード")
            .ToTable("担当者マスタ").PrimaryColumn("担当者コード");

        // インデックス
        Create.Index("idx_担当者_部門コード").OnTable("担当者マスタ").OnColumn("部門コード");
        Create.Index("idx_工数実績_作業指示番号").OnTable("工数実績データ").OnColumn("作業指示番号");
        Create.Index("idx_工数実績_品目コード").OnTable("工数実績データ").OnColumn("品目コード");
        Create.Index("idx_工数実績_工程コード").OnTable("工数実績データ").OnColumn("工程コード");
        Create.Index("idx_工数実績_担当者コード").OnTable("工数実績データ").OnColumn("担当者コード");
        Create.Index("idx_工数実績_作業日").OnTable("工数実績データ").OnColumn("作業日");
    }

    public override void Down()
    {
        Delete.Table("工数実績データ");
        Delete.Table("担当者マスタ");
        Delete.Table("部門マスタ");
    }
}
