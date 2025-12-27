using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 出荷検査テーブルの作成
/// </summary>
[Migration(20)]
public class V020_CreateShipmentInspectionTables : Migration
{
    public override void Up()
    {
        // 検査判定 ENUM
        Execute.Sql("CREATE TYPE \"検査判定\" AS ENUM ('合格', '不合格', '保留')");

        // 欠点マスタは V010 で作成済み

        // 出荷検査データ
        Create.Table("出荷検査データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("出荷検査番号").AsString(20).NotNullable().Unique()
            .WithColumn("出荷番号").AsString(20).NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("検査日").AsDate().NotNullable()
            .WithColumn("検査担当者コード").AsString(20).NotNullable()
            .WithColumn("検査数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("合格数").AsDecimal(15, 2).NotNullable()
            .WithColumn("不合格数").AsDecimal(15, 2).NotNullable()
            .WithColumn("判定").AsCustom("\"検査判定\"").NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 出荷検査結果データ
        Create.Table("出荷検査結果データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("出荷検査番号").AsString(20).NotNullable()
            .WithColumn("欠点コード").AsString(20).NotNullable()
            .WithColumn("数量").AsDecimal(15, 2).NotNullable();

        // 外部キー
        Create.ForeignKey("fk_出荷検査結果_出荷検査")
            .FromTable("出荷検査結果データ").ForeignColumn("出荷検査番号")
            .ToTable("出荷検査データ").PrimaryColumn("出荷検査番号");

        Create.ForeignKey("fk_出荷検査結果_欠点")
            .FromTable("出荷検査結果データ").ForeignColumn("欠点コード")
            .ToTable("欠点マスタ").PrimaryColumn("欠点コード");

        // ユニーク制約
        Create.UniqueConstraint("uq_出荷検査結果")
            .OnTable("出荷検査結果データ").Columns("出荷検査番号", "欠点コード");

        // インデックス
        Create.Index("idx_出荷検査_検査日").OnTable("出荷検査データ").OnColumn("検査日");
        Create.Index("idx_出荷検査_出荷番号").OnTable("出荷検査データ").OnColumn("出荷番号");
    }

    public override void Down()
    {
        Delete.Table("出荷検査結果データ");
        Delete.Table("出荷検査データ");
        // 欠点マスタは V010 で作成されるため、ここでは削除しない
        Execute.Sql("DROP TYPE \"検査判定\"");
    }
}
