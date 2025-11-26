using FluentMigrator;

namespace AccountingSystem.Infrastructure.Migrations
{
    /// <summary>
    /// 初期セットアップマイグレーション
    /// </summary>
    [Migration(20250121000)]
    public class Migration_20250121_000_InitialSetup : Migration
    {
        public override void Up()
        {
            // PostgreSQL用のタイムゾーン設定
            Execute.Sql("SET timezone = 'Asia/Tokyo'");
        }

        public override void Down()
        {
            // ロールバック処理は不要
        }
    }
}
