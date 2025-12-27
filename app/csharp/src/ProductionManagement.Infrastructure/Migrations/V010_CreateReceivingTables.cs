using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

[Migration(10)]
public class V010_CreateReceivingTables : Migration
{
    public override void Up()
    {
        // 検査結果区分 ENUM
        Execute.Sql(@"
            CREATE TYPE 検査結果区分 AS ENUM (
                '合格', '不合格', '条件付合格'
            );
        ");

        // 入荷受入データ
        Create.Table("入荷受入データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("入荷番号").AsString(20).NotNullable().Unique()
            .WithColumn("発注番号").AsString(20).NotNullable()
            .WithColumn("発注行番号").AsInt32().NotNullable()
            .WithColumn("入荷日").AsDate().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("入荷数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("納品書番号").AsString(50).Nullable()
            .WithColumn("ロット番号").AsString(50).Nullable()
            .WithColumn("入荷場所コード").AsString(20).Nullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Execute.Sql(@"
            ALTER TABLE ""入荷受入データ""
            ADD COLUMN ""入荷区分"" 入荷受入区分 NOT NULL DEFAULT '通常入荷';
        ");

        // Note: 発注明細データへのFK（発注番号, 発注行番号の複合キーへの参照）
        Execute.Sql(@"
            ALTER TABLE ""入荷受入データ""
            ADD CONSTRAINT fk_入荷受入_発注明細
            FOREIGN KEY (""発注番号"", ""発注行番号"")
            REFERENCES ""発注明細データ"" (""発注番号"", ""発注行番号"");
        ");

        // 欠点マスタ
        Create.Table("欠点マスタ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("欠点コード").AsString(20).NotNullable().Unique()
            .WithColumn("欠点名").AsString(100).NotNullable()
            .WithColumn("欠点区分").AsString(20).Nullable()
            .WithColumn("表示順").AsInt32().NotNullable().WithDefaultValue(0)
            .WithColumn("有効フラグ").AsBoolean().NotNullable().WithDefaultValue(true)
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        // 受入検査データ
        Create.Table("受入検査データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("検査番号").AsString(20).NotNullable().Unique()
            .WithColumn("入荷番号").AsString(20).NotNullable()
            .WithColumn("検査日").AsDate().NotNullable()
            .WithColumn("検査数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("合格数量").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("不合格数量").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("欠点コード").AsString(20).Nullable()
            .WithColumn("検査担当者コード").AsString(20).Nullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Execute.Sql(@"
            ALTER TABLE ""受入検査データ""
            ADD COLUMN ""検査結果"" 検査結果区分 NOT NULL DEFAULT '合格';
        ");

        Create.ForeignKey("fk_受入検査_入荷")
            .FromTable("受入検査データ").ForeignColumn("入荷番号")
            .ToTable("入荷受入データ").PrimaryColumn("入荷番号");

        Create.ForeignKey("fk_受入検査_欠点")
            .FromTable("受入検査データ").ForeignColumn("欠点コード")
            .ToTable("欠点マスタ").PrimaryColumn("欠点コード");

        // 検収データ
        Create.Table("検収データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("検収番号").AsString(20).NotNullable().Unique()
            .WithColumn("検査番号").AsString(20).NotNullable()
            .WithColumn("検収日").AsDate().NotNullable()
            .WithColumn("検収数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("検収金額").AsDecimal(15, 2).NotNullable()
            .WithColumn("消費税金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("入庫場所コード").AsString(20).Nullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Create.ForeignKey("fk_検収_検査")
            .FromTable("検収データ").ForeignColumn("検査番号")
            .ToTable("受入検査データ").PrimaryColumn("検査番号");

        // インデックス
        Create.Index("idx_入荷受入_発注番号").OnTable("入荷受入データ").OnColumn("発注番号");
        Create.Index("idx_入荷受入_入荷日").OnTable("入荷受入データ").OnColumn("入荷日");
        Create.Index("idx_入荷受入_品目コード").OnTable("入荷受入データ").OnColumn("品目コード");
        Create.Index("idx_受入検査_入荷番号").OnTable("受入検査データ").OnColumn("入荷番号");
        Create.Index("idx_受入検査_検査日").OnTable("受入検査データ").OnColumn("検査日");
        Create.Index("idx_検収_検査番号").OnTable("検収データ").OnColumn("検査番号");
        Create.Index("idx_検収_検収日").OnTable("検収データ").OnColumn("検収日");
    }

    public override void Down()
    {
        Delete.Table("検収データ");
        Delete.Table("受入検査データ");
        Delete.Table("欠点マスタ");
        Delete.Table("入荷受入データ");
        Execute.Sql("DROP TYPE IF EXISTS 検査結果区分;");
    }
}
