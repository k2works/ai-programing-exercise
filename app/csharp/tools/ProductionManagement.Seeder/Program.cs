using Microsoft.Extensions.Logging;
using Npgsql;
using ProductionManagement.Infrastructure.Seed;

// 接続文字列を環境変数または引数から取得
var connectionString = args.Length > 0
    ? args[0]
    : Environment.GetEnvironmentVariable("DATABASE_URL")
      ?? "Host=localhost;Port=5432;Database=production_management;Username=postgres;Password=postgres";

Console.WriteLine("=== ProductionManagement Database Seeder ===");
Console.WriteLine($"Connection: {MaskPassword(connectionString)}");
Console.WriteLine();

try
{
    Console.WriteLine("Seeding data...");

    using var loggerFactory = LoggerFactory.Create(builder =>
    {
        builder.AddConsole();
        builder.SetMinimumLevel(LogLevel.Information);
    });

    await using var connection = new NpgsqlConnection(connectionString);
    await connection.OpenAsync();

    var masterSeeder = new MasterDataSeeder(
        loggerFactory.CreateLogger<MasterDataSeeder>(),
        connection);

    var transactionSeeder = new TransactionDataSeeder(
        loggerFactory.CreateLogger<TransactionDataSeeder>(),
        connection);

    var seedDataService = new SeedDataService(
        loggerFactory.CreateLogger<SeedDataService>(),
        connection,
        masterSeeder,
        transactionSeeder);

    await seedDataService.SeedAllAsync();

    Console.WriteLine();
    Console.WriteLine("Seeding completed successfully!");
    return 0;
}
catch (Exception ex)
{
    Console.Error.WriteLine($"Seeding failed: {ex.Message}");
    Console.Error.WriteLine(ex.StackTrace);
    return 1;
}

static string MaskPassword(string connectionString)
{
    // パスワードをマスクして表示
    return System.Text.RegularExpressions.Regex.Replace(
        connectionString,
        @"(Password|Pwd)=([^;]*)",
        "$1=****",
        System.Text.RegularExpressions.RegexOptions.IgnoreCase);
}
