using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 取引先マスタテーブルの作成
/// </summary>
[Migration(6)]
public class V006_CreateSupplierMaster : Migration
{
    public override void Up()
    {
        // 取引先区分 ENUM
        Execute.Sql("CREATE TYPE 取引先区分 AS ENUM ('仕入先', '外注先', '得意先', '仕入先兼外注先')");

        // 取引先マスタ
        Execute.Sql("""
            CREATE TABLE "取引先マスタ" (
                "取引先コード" VARCHAR(20) NOT NULL,
                "適用開始日" DATE NOT NULL,
                "適用停止日" DATE,
                "取引先名" VARCHAR(100) NOT NULL,
                "取引先カナ" VARCHAR(100),
                "取引先区分" 取引先区分 NOT NULL,
                "郵便番号" VARCHAR(10),
                "住所" VARCHAR(200),
                "電話番号" VARCHAR(20),
                "FAX番号" VARCHAR(20),
                "担当者名" VARCHAR(50),
                "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY ("取引先コード", "適用開始日")
            )
            """);

        Execute.Sql("""CREATE INDEX idx_取引先マスタ_取引先区分 ON "取引先マスタ"("取引先区分")""");
    }

    public override void Down()
    {
        Execute.Sql("""DROP TABLE IF EXISTS "取引先マスタ" CASCADE""");
        Execute.Sql("DROP TYPE IF EXISTS 取引先区分");
    }
}
