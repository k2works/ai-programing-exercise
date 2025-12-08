namespace FinancialAccounting.Infrastructure

open System
open System.Reflection
open Microsoft.Extensions.DependencyInjection
open FluentMigrator.Runner
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Infrastructure.Persistence.Repositories

/// <summary>
/// Infrastructure 層の DI 設定
/// </summary>
module DependencyInjection =

    /// <summary>
    /// FluentMigrator を設定
    /// </summary>
    let addFluentMigrator (services: IServiceCollection) (connectionString: string) =
        services
            .AddFluentMigratorCore()
            .ConfigureRunner(fun rb ->
                rb.AddPostgres()
                  .WithGlobalConnectionString(connectionString)
                  .ScanIn(Assembly.GetExecutingAssembly()).For.Migrations()
                |> ignore)
            .AddLogging(fun lb -> lb.AddFluentMigratorConsole() |> ignore)
        |> ignore
        services

    /// <summary>
    /// リポジトリを登録
    /// </summary>
    let addRepositories (services: IServiceCollection) (connectionString: string) =
        services.AddScoped<IJournalRepository>(fun _ ->
            JournalRepository(connectionString) :> IJournalRepository)
        |> ignore
        services

    /// <summary>
    /// マイグレーションを実行
    /// </summary>
    let runMigrations (serviceProvider: IServiceProvider) =
        use scope = serviceProvider.CreateScope()
        let runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>()
        runner.MigrateUp()
