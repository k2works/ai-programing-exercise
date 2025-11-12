using Microsoft.Extensions.Configuration;
using SalesManagement.Infrastructure;

// 設定ファイルの読み込み
var configuration = new ConfigurationBuilder()
    .SetBasePath(Directory.GetCurrentDirectory())
    .AddJsonFile("appsettings.json", optional: false)
    .Build();

var databaseType = configuration["DatabaseType"] ?? "PostgreSQL";
var connectionString = configuration.GetConnectionString(databaseType)
    ?? throw new InvalidOperationException($"接続文字列が設定されていません: {databaseType}");

Console.WriteLine($"=== マイグレーション実行 ({databaseType}) ===");
try
{
    MigrationRunner.MigrateDatabase(connectionString, databaseType);
    Console.WriteLine("マイグレーション完了\n");
}
catch (Exception ex)
{
    Console.WriteLine($"マイグレーションエラー: {ex.Message}");
    Console.WriteLine(ex.StackTrace);
    return;
}

// DataSeederの実行
var seeder = new DataSeeder(connectionString, databaseType);

try
{
    await seeder.SeedAllAsync();
    Console.WriteLine("\nSeedデータの投入が完了しました。");
}
catch (Exception ex)
{
    Console.WriteLine($"\nエラーが発生しました: {ex.Message}");
    Console.WriteLine(ex.StackTrace);
}
