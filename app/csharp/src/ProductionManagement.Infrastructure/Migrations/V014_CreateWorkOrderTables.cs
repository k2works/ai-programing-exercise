using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 作業指示テーブルの作成
/// </summary>
[Migration(14)]
public class V014_CreateWorkOrderTables : Migration
{
    public override void Up()
    {
        // 作業指示ステータス ENUM
        Execute.Sql("CREATE TYPE 作業指示ステータス AS ENUM ('未着手', '作業中', '完了', '中断')");

        // 作業指示データ
        Create.Table("作業指示データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("作業指示番号").AsString(20).NotNullable().Unique()
            .WithColumn("オーダ番号").AsString(20).NotNullable()
            .WithColumn("作業指示日").AsDate().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("作業指示数").AsDecimal(15, 2).NotNullable()
            .WithColumn("場所コード").AsString(20).NotNullable()
            .WithColumn("開始予定日").AsDate().NotNullable()
            .WithColumn("完成予定日").AsDate().NotNullable()
            .WithColumn("実績開始日").AsDate().Nullable()
            .WithColumn("実績完了日").AsDate().Nullable()
            .WithColumn("完成済数").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("総良品数").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("総不良品数").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("ステータス").AsCustom("作業指示ステータス").NotNullable().WithDefaultValue("未着手")
            .WithColumn("完了フラグ").AsBoolean().NotNullable().WithDefaultValue(false)
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        // 作業指示明細データ
        Create.Table("作業指示明細データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("作業指示番号").AsString(20).NotNullable()
            .WithColumn("工順").AsInt32().NotNullable()
            .WithColumn("工程コード").AsString(20).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 外部キー
        Create.ForeignKey("fk_作業指示_オーダ")
            .FromTable("作業指示データ").ForeignColumn("オーダ番号")
            .ToTable("オーダ情報").PrimaryColumn("オーダNO");

        Create.ForeignKey("fk_作業指示明細_作業指示")
            .FromTable("作業指示明細データ").ForeignColumn("作業指示番号")
            .ToTable("作業指示データ").PrimaryColumn("作業指示番号");

        Create.ForeignKey("fk_作業指示明細_工程")
            .FromTable("作業指示明細データ").ForeignColumn("工程コード")
            .ToTable("工程マスタ").PrimaryColumn("工程コード");

        // ユニーク制約
        Create.UniqueConstraint("uq_作業指示明細_番号_工順")
            .OnTable("作業指示明細データ")
            .Columns("作業指示番号", "工順");

        // インデックス
        Create.Index("idx_作業指示_オーダ番号").OnTable("作業指示データ").OnColumn("オーダ番号");
        Create.Index("idx_作業指示_品目コード").OnTable("作業指示データ").OnColumn("品目コード");
        Create.Index("idx_作業指示_ステータス").OnTable("作業指示データ").OnColumn("ステータス");
        Create.Index("idx_作業指示明細_工程コード").OnTable("作業指示明細データ").OnColumn("工程コード");
    }

    public override void Down()
    {
        Delete.Table("作業指示明細データ");
        Delete.Table("作業指示データ");
        Execute.Sql("DROP TYPE 作業指示ステータス");
    }
}
