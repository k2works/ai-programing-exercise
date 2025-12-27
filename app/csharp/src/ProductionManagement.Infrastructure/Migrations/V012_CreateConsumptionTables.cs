using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

[Migration(12)]
public class V012_CreateConsumptionTables : Migration
{
    public override void Up()
    {
        // 消費データ
        Create.Table("消費データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("消費番号").AsString(20).NotNullable().Unique()
            .WithColumn("入荷番号").AsString(20).NotNullable()
            .WithColumn("消費日").AsDate().NotNullable()
            .WithColumn("取引先コード").AsString(20).NotNullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Create.ForeignKey("fk_消費_入荷")
            .FromTable("消費データ").ForeignColumn("入荷番号")
            .ToTable("入荷受入データ").PrimaryColumn("入荷番号");

        // Note: 取引先マスタへのFKは複合キー（取引先コード + 適用開始日）のため削除
        // アプリケーション層でバリデーション

        // 消費明細データ
        Create.Table("消費明細データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("消費番号").AsString(20).NotNullable()
            .WithColumn("消費行番号").AsInt32().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("消費数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.ForeignKey("fk_消費明細_消費")
            .FromTable("消費明細データ").ForeignColumn("消費番号")
            .ToTable("消費データ").PrimaryColumn("消費番号");

        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除
        // アプリケーション層でバリデーション

        Create.UniqueConstraint("uq_消費明細_消費番号行番号")
            .OnTable("消費明細データ")
            .Columns("消費番号", "消費行番号");

        // インデックス
        Create.Index("idx_消費データ_入荷番号").OnTable("消費データ").OnColumn("入荷番号");
        Create.Index("idx_消費データ_取引先コード").OnTable("消費データ").OnColumn("取引先コード");
        Create.Index("idx_消費データ_消費日").OnTable("消費データ").OnColumn("消費日");
        Create.Index("idx_消費明細_品目コード").OnTable("消費明細データ").OnColumn("品目コード");
    }

    public override void Down()
    {
        Delete.Table("消費明細データ");
        Delete.Table("消費データ");
    }
}
