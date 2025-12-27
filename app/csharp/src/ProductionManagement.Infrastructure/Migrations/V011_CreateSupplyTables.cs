using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

[Migration(11)]
public class V011_CreateSupplyTables : Migration
{
    public override void Up()
    {
        // 支給区分 ENUM
        Execute.Sql(@"
            CREATE TYPE 支給区分 AS ENUM ('有償支給', '無償支給');
        ");

        // 支給データ
        Create.Table("支給データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("支給番号").AsString(20).NotNullable().Unique()
            .WithColumn("発注番号").AsString(20).NotNullable()
            .WithColumn("発注行番号").AsInt32().NotNullable()
            .WithColumn("取引先コード").AsString(20).NotNullable()
            .WithColumn("支給日").AsDate().NotNullable()
            .WithColumn("支給担当者コード").AsString(20).NotNullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("作成者").AsString(50).Nullable()
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新者").AsString(50).Nullable();

        Execute.Sql(@"
            ALTER TABLE ""支給データ""
            ADD COLUMN ""支給区分"" 支給区分 NOT NULL DEFAULT '無償支給';
        ");

        // 発注明細データへのFK（複合キー）
        Execute.Sql(@"
            ALTER TABLE ""支給データ""
            ADD CONSTRAINT fk_支給_発注明細
            FOREIGN KEY (""発注番号"", ""発注行番号"")
            REFERENCES ""発注明細データ"" (""発注番号"", ""発注行番号"");
        ");

        // Note: 取引先マスタへのFKは複合キー（取引先コード + 適用開始日）のため削除
        // アプリケーション層でバリデーション

        // 支給明細データ
        Create.Table("支給明細データ")
            .WithColumn("ID").AsInt32().PrimaryKey().Identity()
            .WithColumn("支給番号").AsString(20).NotNullable()
            .WithColumn("支給行番号").AsInt32().NotNullable()
            .WithColumn("品目コード").AsString(20).NotNullable()
            .WithColumn("支給数").AsDecimal(15, 2).NotNullable()
            .WithColumn("支給単価").AsDecimal(15, 2).NotNullable()
            .WithColumn("支給金額").AsDecimal(15, 2).NotNullable()
            .WithColumn("備考").AsString().Nullable()
            .WithColumn("作成日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime)
            .WithColumn("更新日時").AsDateTime().NotNullable().WithDefault(SystemMethods.CurrentDateTime);

        Create.ForeignKey("fk_支給明細_支給")
            .FromTable("支給明細データ").ForeignColumn("支給番号")
            .ToTable("支給データ").PrimaryColumn("支給番号");

        // Note: 品目マスタへのFKは複合キー（品目コード + 適用開始日）のため削除
        // アプリケーション層でバリデーション

        Create.UniqueConstraint("uq_支給明細_支給番号行番号")
            .OnTable("支給明細データ")
            .Columns("支給番号", "支給行番号");

        // インデックス
        Create.Index("idx_支給データ_発注番号").OnTable("支給データ")
            .OnColumn("発注番号").Ascending()
            .OnColumn("発注行番号").Ascending();
        Create.Index("idx_支給データ_取引先コード").OnTable("支給データ").OnColumn("取引先コード");
        Create.Index("idx_支給データ_支給日").OnTable("支給データ").OnColumn("支給日");
        Create.Index("idx_支給明細_品目コード").OnTable("支給明細データ").OnColumn("品目コード");
    }

    public override void Down()
    {
        Delete.Table("支給明細データ");
        Delete.Table("支給データ");
        Execute.Sql("DROP TYPE IF EXISTS 支給区分;");
    }
}
