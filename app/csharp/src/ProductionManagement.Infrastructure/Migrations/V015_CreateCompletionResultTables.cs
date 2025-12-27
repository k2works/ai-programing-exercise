using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 完成実績関連テーブルの作成
/// </summary>
[Migration(15)]
public class V015_CreateCompletionResultTables : Migration
{
    public override void Up()
    {
        // 完成実績データ
        Create.Table("完成実績データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("完成実績番号").AsString(20).NotNullable().Unique()
            .WithColumn("作業指示番号").AsString(20).NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("完成日").AsDate().NotNullable()
            .WithColumn("完成数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("良品数").AsDecimal(15, 2).NotNullable()
            .WithColumn("不良品数").AsDecimal(15, 2).NotNullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        // 完成検査結果データ
        Create.Table("完成検査結果データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("完成実績番号").AsString(20).NotNullable()
            .WithColumn("欠点コード").AsString(20).NotNullable()
            .WithColumn("数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 外部キー
        Create.ForeignKey("fk_完成実績_作業指示")
            .FromTable("完成実績データ").ForeignColumn("作業指示番号")
            .ToTable("作業指示データ").PrimaryColumn("作業指示番号");

        // 品目マスタへのFK: 複合キー（品目コード、有効開始日）のため削除

        Create.ForeignKey("fk_完成検査結果_完成実績")
            .FromTable("完成検査結果データ").ForeignColumn("完成実績番号")
            .ToTable("完成実績データ").PrimaryColumn("完成実績番号");

        Create.ForeignKey("fk_完成検査結果_欠点")
            .FromTable("完成検査結果データ").ForeignColumn("欠点コード")
            .ToTable("欠点マスタ").PrimaryColumn("欠点コード");

        // ユニーク制約
        Create.UniqueConstraint("uq_完成検査結果_番号_欠点")
            .OnTable("完成検査結果データ")
            .Columns("完成実績番号", "欠点コード");

        // インデックス
        Create.Index("idx_完成実績_作業指示番号").OnTable("完成実績データ").OnColumn("作業指示番号");
        Create.Index("idx_完成実績_品目コード").OnTable("完成実績データ").OnColumn("品目コード");
        Create.Index("idx_完成実績_完成日").OnTable("完成実績データ").OnColumn("完成日");
    }

    public override void Down()
    {
        Delete.Table("完成検査結果データ");
        Delete.Table("完成実績データ");
    }
}
