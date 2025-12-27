using Microsoft.Extensions.DependencyInjection;
using Npgsql;

namespace ProductionManagement.Infrastructure.Seed;

/// <summary>
/// Seedサービス登録拡張メソッド
/// </summary>
public static class SeedServiceExtensions
{
    /// <summary>
    /// Seedサービスを登録する
    /// </summary>
    /// <param name="services">サービスコレクション</param>
    /// <param name="connectionString">データベース接続文字列</param>
    /// <returns>サービスコレクション</returns>
    public static IServiceCollection AddSeedServices(
        this IServiceCollection services,
        string connectionString)
    {
        services.AddSingleton(_ => new NpgsqlConnection(connectionString));
        services.AddSingleton<MasterDataSeeder>();
        services.AddSingleton<TransactionDataSeeder>();
        services.AddSingleton<SeedDataService>();
        services.AddHostedService<SeedRunner>();

        return services;
    }
}
