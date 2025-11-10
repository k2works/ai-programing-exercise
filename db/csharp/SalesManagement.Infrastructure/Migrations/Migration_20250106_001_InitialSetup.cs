using FluentMigrator;

namespace SalesManagement.Infrastructure.Migrations
{
    /// <summary>
    /// 初期セットアップマイグレーション
    /// </summary>
    [Migration(20250106001)]
    public class Migration20250106001InitialSetup : Migration
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
