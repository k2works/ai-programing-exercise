using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 場所マスタテーブルの作成
/// </summary>
[Migration(5)]
public class V005_CreateLocationMaster : Migration
{
    public override void Up()
    {
        // 場所区分 ENUM
        Execute.Sql("CREATE TYPE 場所区分 AS ENUM ('倉庫', '製造', '検査', '出荷', '外注')");

        // 場所マスタ
        Execute.Sql("""
            CREATE TABLE "場所マスタ" (
                "場所コード" VARCHAR(20) PRIMARY KEY,
                "場所名" VARCHAR(100) NOT NULL,
                "場所区分" 場所区分 NOT NULL,
                "親場所コード" VARCHAR(20) REFERENCES "場所マスタ"("場所コード"),
                "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
                "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP
            )
            """);

        Execute.Sql("""CREATE INDEX idx_場所マスタ_場所区分 ON "場所マスタ"("場所区分")""");
    }

    public override void Down()
    {
        Execute.Sql("""DROP TABLE IF EXISTS "場所マスタ" CASCADE""");
        Execute.Sql("DROP TYPE IF EXISTS 場所区分");
    }
}
