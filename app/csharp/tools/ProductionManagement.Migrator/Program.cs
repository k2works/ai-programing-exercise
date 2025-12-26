using ProductionManagement.Infrastructure.Database;

// 接続文字列を環境変数または引数から取得
var connectionString = args.Length > 0
    ? args[0]
    : Environment.GetEnvironmentVariable("DATABASE_URL")
      ?? "Host=localhost;Port=5432;Database=production_management;Username=postgres;Password=postgres";

Console.WriteLine("=== ProductionManagement Database Migration ===");
Console.WriteLine($"Connection: {MaskPassword(connectionString)}");
Console.WriteLine();

try
{
    Console.WriteLine("Running migrations...");
    MigrationRunner.RunMigrations(connectionString);
    Console.WriteLine();
    Console.WriteLine("Migrations completed successfully!");
    return 0;
}
catch (Exception ex)
{
    Console.Error.WriteLine($"Migration failed: {ex.Message}");
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
