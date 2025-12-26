using FluentMigrator;

namespace ProductionManagement.Infrastructure.Migrations;

/// <summary>
/// 初期セットアップマイグレーション - ENUM型の作成
/// </summary>
[Migration(1)]
public class V001_CreateEnumTypes : Migration
{
    public override void Up()
    {
        // PostgreSQL用のタイムゾーン設定
        Execute.Sql("SET timezone = 'Asia/Tokyo'");

        // 品目区分 ENUM の作成
        Execute.Sql("CREATE TYPE 品目区分 AS ENUM ('製品', '半製品', '中間品', '部品', '材料', '原料', '資材')");
    }

    public override void Down()
    {
        Execute.Sql("DROP TYPE IF EXISTS 品目区分");
    }
}
