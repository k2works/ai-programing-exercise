using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 部品構成表（BOM）テーブルの作成
/// </summary>
[Migration(3)]
public class V003_CreateBom : Migration
{
    public override void Up()
    {
        Execute.Sql("""
            CREATE TABLE "部品構成表" (
                "親品目コード" VARCHAR(20) NOT NULL,
                "子品目コード" VARCHAR(20) NOT NULL,
                "適用開始日" DATE NOT NULL,
                "適用停止日" DATE,
                "基準数量" DECIMAL(15, 2) NOT NULL DEFAULT 1,
                "必要数量" DECIMAL(15, 2) NOT NULL,
                "不良率" DECIMAL(5, 2) DEFAULT 0,
                "工順" INTEGER,
                "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                PRIMARY KEY ("親品目コード", "子品目コード", "適用開始日")
            )
            """);

        // インデックス
        Execute.Sql("""CREATE INDEX idx_bom_親品目コード ON "部品構成表"("親品目コード")""");
        Execute.Sql("""CREATE INDEX idx_bom_子品目コード ON "部品構成表"("子品目コード")""");
    }

    public override void Down()
    {
        Execute.Sql("""DROP TABLE IF EXISTS "部品構成表" CASCADE""");
    }
}
