using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

[Migration(9)]
public class V009_CreatePurchasingTables : Migration
{
    public override void Up()
    {
        // 発注ステータス ENUM
        Execute.Sql(@"
            CREATE TYPE 発注ステータス AS ENUM (
                '作成中', '発注済', '一部入荷', '入荷完了', '検収完了', '取消'
            );
        ");

        // 入荷受入区分 ENUM
        Execute.Sql(@"
            CREATE TYPE 入荷受入区分 AS ENUM (
                '通常入荷', '分割入荷', '返品入荷'
            );
        ");

        // 単価マスタ
        Create.Table("単価マスタ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("取引先コード").AsString(20).NotNullable()
            .WithColumn("ロット単位数").AsDecimal(15, 2).NotNullable().WithDefaultValue(1)
            .WithColumn("使用開始日").AsDate().NotNullable()
            .WithColumn("使用停止日").AsDate().Nullable()
            .WithColumn("単価").AsDecimal(15, 2).NotNullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        // Note: 品目マスタ、取引先マスタは複合キーのためFK制約なし（アプリケーションレベルで整合性を保証）

        Create.UniqueConstraint("uq_単価マスタ_品目取引先開始日")
            .OnTable("単価マスタ")
            .Columns("品目コード", "取引先コード", "ロット単位数", "使用開始日");

        // 発注データ
        Create.Table("発注データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("発注番号").AsString(20).NotNullable().Unique()
            .WithColumn("発注日").AsDate().NotNullable()
            .WithColumn("取引先コード").AsString(20).NotNullable()
            .WithColumn("発注担当者コード").AsString(20).Nullable()
            .WithColumn("発注部門コード").AsString(20).Nullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Execute.Sql(@"
            ALTER TABLE ""発注データ""
            ADD COLUMN ""ステータス"" 発注ステータス NOT NULL DEFAULT '作成中';
        ");

        // Note: 取引先マスタは複合キーのためFK制約なし

        // 発注明細データ
        Create.Table("発注明細データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("発注番号").AsString(20).NotNullable()
            .WithColumn("発注行番号").AsInt32().NotNullable()
            .WithColumn("オーダNO").AsString(20).Nullable()
            .WithColumn("納入場所コード").AsString(20).Nullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("諸口品目区分").AsBoolean().NotNullable().WithDefaultValue(false)
            .WithColumn("受入予定日").AsDate().NotNullable()
            .WithColumn("回答納期").AsDate().Nullable()
            .WithColumn("発注単価").AsDecimal(15, 2).NotNullable()
            .WithColumn("発注数量").AsDecimal(15, 2).NotNullable()
            .WithColumn("入荷済数量").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("検査済数量").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("検収済数量").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("発注金額").AsDecimal(15, 2).NotNullable()
            .WithColumn("消費税金額").AsDecimal(15, 2).NotNullable().WithDefaultValue(0)
            .WithColumn("完了フラグ").AsBoolean().NotNullable().WithDefaultValue(false)
            .WithColumn("明細備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Create.ForeignKey("fk_発注明細_発注")
            .FromTable("発注明細データ").ForeignColumn("発注番号")
            .ToTable("発注データ").PrimaryColumn("発注番号");

        // Note: 品目マスタは複合キーのためFK制約なし

        Create.UniqueConstraint("uq_発注明細_発注番号行番号")
            .OnTable("発注明細データ")
            .Columns("発注番号", "発注行番号");

        // 諸口品目情報（マスタに登録されていない臨時品目）
        Create.Table("諸口品目情報")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("発注番号").AsString(20).NotNullable()
            .WithColumn("発注行番号").AsInt32().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("品名").AsString(100).NotNullable()
            .WithColumn("規格").AsString(100).Nullable()
            .WithColumn("図番メーカー").AsString(100).Nullable()
            .WithColumn("版数").AsString(20).Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.UniqueConstraint("uq_諸口品目情報")
            .OnTable("諸口品目情報")
            .Columns("発注番号", "発注行番号", "品目コード");

        // インデックス
        Create.Index("idx_発注データ_取引先コード").OnTable("発注データ").OnColumn("取引先コード");
        Create.Index("idx_発注データ_発注日").OnTable("発注データ").OnColumn("発注日");
        Create.Index("idx_発注明細_発注番号").OnTable("発注明細データ").OnColumn("発注番号");
        Create.Index("idx_発注明細_品目コード").OnTable("発注明細データ").OnColumn("品目コード");
        Create.Index("idx_単価マスタ_品目取引先").OnTable("単価マスタ").OnColumn("品目コード").Ascending().OnColumn("取引先コード").Ascending();
    }

    public override void Down()
    {
        Delete.Table("諸口品目情報");
        Delete.Table("発注明細データ");
        Delete.Table("発注データ");
        Delete.Table("単価マスタ");
        Execute.Sql("DROP TYPE IF EXISTS 入荷受入区分;");
        Execute.Sql("DROP TYPE IF EXISTS 発注ステータス;");
    }
}
