using Microsoft.Extensions.Configuration;
using SalesManagement.Infrastructure;

var configuration = new ConfigurationBuilder()
    .SetBasePath(Directory.GetCurrentDirectory())
    .AddJsonFile("appsettings.json", optional: false)
    .Build();

var databaseType = configuration["DatabaseType"] ?? "PostgreSQL";
var connectionString = configuration.GetConnectionString(databaseType);

if (string.IsNullOrEmpty(connectionString))
{
    Console.WriteLine($"接続文字列が見つかりません: {databaseType}");
    return;
}

Console.WriteLine($"データベースマイグレーション開始: {databaseType}");
MigrationRunner.MigrateDatabase(connectionString, databaseType);
Console.WriteLine("マイグレーション完了");
