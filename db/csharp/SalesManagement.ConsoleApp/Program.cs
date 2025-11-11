using Microsoft.Extensions.Configuration;
using SalesManagement.Infrastructure;

// 設定ファイルの読み込み
var configuration = new ConfigurationBuilder()
    .SetBasePath(Directory.GetCurrentDirectory())
    .AddJsonFile("appsettings.json", optional: false)
    .Build();

var connectionString = configuration.GetConnectionString("DefaultConnection")
    ?? throw new InvalidOperationException("接続文字列が設定されていません");

// DataSeederの実行
var seeder = new DataSeeder(connectionString);

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
