namespace SalesManagement.Infrastructure.Migrations

open FluentMigrator

/// <summary>
/// 初期セットアップマイグレーション
/// </summary>
[<Migration(20250106001L)>]
type Migration_20250106_001_InitialSetup() =
    inherit Migration()

    override this.Up() =
        // PostgreSQL用のタイムゾーン設定
        this.Execute.Sql("SET timezone = 'Asia/Tokyo'")

    override this.Down() =
        // ロールバック処理は不要
        ()
