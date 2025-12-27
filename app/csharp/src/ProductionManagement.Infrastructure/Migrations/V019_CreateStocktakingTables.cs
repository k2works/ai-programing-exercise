using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 棚卸テーブルの作成
/// </summary>
[Migration(19)]
public class V019_CreateStocktakingTables : Migration
{
    public override void Up()
    {
        // 棚卸ステータス ENUM
        Execute.Sql("CREATE TYPE \"棚卸ステータス\" AS ENUM ('発行済', '入力済', '確定')");

        // 棚卸データ
        Create.Table("棚卸データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("棚卸番号").AsString(20).NotNullable().Unique()
            .WithColumn("場所コード").AsString(20).NotNullable()
            .WithColumn("棚卸日").AsDate().NotNullable()
            .WithColumn("ステータス").AsCustom("\"棚卸ステータス\"").NotNullable().WithDefaultValue("発行済")
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 棚卸明細データ
        Create.Table("棚卸明細データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("棚卸番号").AsString(20).NotNullable()
            .WithColumn("棚卸行番号").AsInt32().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("帳簿数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("実棚数量").AsDecimal(15, 2).Nullable()
            .WithColumn("差異数量").AsDecimal(15, 2).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // 在庫調整データ
        Create.Table("在庫調整データ")
            .WithColumn("ID").AsInt64().PrimaryKey().Identity()
            .WithColumn("在庫調整番号").AsString(30).NotNullable().Unique()
            .WithColumn("棚卸番号").AsString(20).Nullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("場所コード").AsString(20).NotNullable()
            .WithColumn("調整日").AsDate().NotNullable()
            .WithColumn("調整担当者コード").AsString(20).NotNullable()
            .WithColumn("調整数").AsDecimal(15, 2).NotNullable()
            .WithColumn("理由コード").AsString(20).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        // ユニーク制約
        Create.UniqueConstraint("uq_棚卸明細")
            .OnTable("棚卸明細データ").Columns("棚卸番号", "棚卸行番号");

        // 外部キー
        Create.ForeignKey("fk_棚卸_場所")
            .FromTable("棚卸データ").ForeignColumn("場所コード")
            .ToTable("場所マスタ").PrimaryColumn("場所コード");
        Create.ForeignKey("fk_棚卸明細_棚卸")
            .FromTable("棚卸明細データ").ForeignColumn("棚卸番号")
            .ToTable("棚卸データ").PrimaryColumn("棚卸番号");
        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除

        Create.ForeignKey("fk_在庫調整_棚卸")
            .FromTable("在庫調整データ").ForeignColumn("棚卸番号")
            .ToTable("棚卸データ").PrimaryColumn("棚卸番号");
        Create.ForeignKey("fk_在庫調整_場所")
            .FromTable("在庫調整データ").ForeignColumn("場所コード")
            .ToTable("場所マスタ").PrimaryColumn("場所コード");
        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除

        // インデックス
        Create.Index("idx_棚卸_場所").OnTable("棚卸データ").OnColumn("場所コード");
        Create.Index("idx_棚卸_日付").OnTable("棚卸データ").OnColumn("棚卸日");
        Create.Index("idx_在庫調整_棚卸").OnTable("在庫調整データ").OnColumn("棚卸番号");
    }

    public override void Down()
    {
        Delete.Table("在庫調整データ");
        Delete.Table("棚卸明細データ");
        Delete.Table("棚卸データ");
        Execute.Sql("DROP TYPE \"棚卸ステータス\"");
    }
}
