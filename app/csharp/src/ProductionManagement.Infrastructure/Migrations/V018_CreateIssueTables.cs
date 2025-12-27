using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 払出テーブルの作成
/// </summary>
[Migration(18)]
public class V018_CreateIssueTables : Migration
{
    public override void Up()
    {
        // 払出指示データ
        Create.Table("払出指示データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("払出指示番号").AsString(20).NotNullable().Unique()
            .WithColumn("オーダ番号").AsString(20).NotNullable()
            .WithColumn("払出指示日").AsDate().NotNullable()
            .WithColumn("場所コード").AsString(20).NotNullable()
            .WithColumn("備考").AsString(500).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 払出指示明細データ
        Create.Table("払出指示明細データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("払出指示番号").AsString(20).NotNullable()
            .WithColumn("払出行番号").AsInt32().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("工順").AsInt32().NotNullable()
            .WithColumn("払出数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 払出データ
        Create.Table("払出データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("払出番号").AsString(20).NotNullable().Unique()
            .WithColumn("作業指示番号").AsString(20).NotNullable()
            .WithColumn("工順").AsInt32().NotNullable()
            .WithColumn("場所コード").AsString(20).NotNullable()
            .WithColumn("払出日").AsDate().NotNullable()
            .WithColumn("払出担当者コード").AsString(20).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 払出明細データ
        Create.Table("払出明細データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("払出番号").AsString(20).NotNullable()
            .WithColumn("払出行番号").AsInt32().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("払出数").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // ユニーク制約
        Create.UniqueConstraint("uq_払出指示明細")
            .OnTable("払出指示明細データ").Columns("払出指示番号", "払出行番号");
        Create.UniqueConstraint("uq_払出明細")
            .OnTable("払出明細データ").Columns("払出番号", "払出行番号");

        // 外部キー
        Create.ForeignKey("fk_払出指示_オーダ")
            .FromTable("払出指示データ").ForeignColumn("オーダ番号")
            .ToTable("オーダ情報").PrimaryColumn("オーダNO");
        Create.ForeignKey("fk_払出指示_場所")
            .FromTable("払出指示データ").ForeignColumn("場所コード")
            .ToTable("場所マスタ").PrimaryColumn("場所コード");
        Create.ForeignKey("fk_払出指示明細_払出指示")
            .FromTable("払出指示明細データ").ForeignColumn("払出指示番号")
            .ToTable("払出指示データ").PrimaryColumn("払出指示番号");

        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除

        Create.ForeignKey("fk_払出_場所")
            .FromTable("払出データ").ForeignColumn("場所コード")
            .ToTable("場所マスタ").PrimaryColumn("場所コード");
        Create.ForeignKey("fk_払出_作業指示")
            .FromTable("払出データ").ForeignColumn("作業指示番号")
            .ToTable("作業指示データ").PrimaryColumn("作業指示番号");
        Create.ForeignKey("fk_払出明細_払出")
            .FromTable("払出明細データ").ForeignColumn("払出番号")
            .ToTable("払出データ").PrimaryColumn("払出番号");

        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除

        // インデックス
        Create.Index("idx_払出指示_オーダ").OnTable("払出指示データ").OnColumn("オーダ番号");
        Create.Index("idx_払出指示_場所").OnTable("払出指示データ").OnColumn("場所コード");
        Create.Index("idx_払出_作業指示").OnTable("払出データ").OnColumn("作業指示番号");
    }

    public override void Down()
    {
        Delete.Table("払出明細データ");
        Delete.Table("払出データ");
        Delete.Table("払出指示明細データ");
        Delete.Table("払出指示データ");
    }
}
