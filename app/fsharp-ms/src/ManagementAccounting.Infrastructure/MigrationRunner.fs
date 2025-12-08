module ManagementAccounting.Infrastructure.MigrationRunner

open FluentMigrator.Runner
open Microsoft.Extensions.DependencyInjection
open System

let migrateDatabase (connectionString: string) =
    let serviceProvider =
        let services = ServiceCollection()

        let configureRunner (rb: IMigrationRunnerBuilder) =
            rb.AddPostgres()
              .WithGlobalConnectionString(connectionString)
              .ScanIn(typeof<Migrations.V001_CreateFinancialAnalysisCacheTable>.Assembly)
              .For.Migrations() |> ignore

        services.AddFluentMigratorCore()
                .ConfigureRunner(Action<IMigrationRunnerBuilder> configureRunner)
                .AddLogging(fun lb -> lb.AddFluentMigratorConsole() |> ignore)
                .BuildServiceProvider(false)

    use scope = serviceProvider.CreateScope()
    let runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>()
    runner.MigrateUp()
