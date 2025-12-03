module AccountingSystem.Infrastructure.MigrationRunner

open FluentMigrator.Runner
open Microsoft.Extensions.DependencyInjection
open System

let migrateDatabase (connectionString: string) (databaseType: string) =
    let serviceProvider =
        let services = ServiceCollection()

        let configureRunner (rb: IMigrationRunnerBuilder) =
            match databaseType with
            | "PostgreSQL" ->
                rb.AddPostgres()
                  .WithGlobalConnectionString(connectionString)
                  .ScanIn(typeof<Migrations.Migration_20250106_001_InitialSetup>.Assembly)
                  .For.Migrations() |> ignore
            | "MySQL" ->
                rb.AddMySql5()
                  .WithGlobalConnectionString(connectionString)
                  .ScanIn(typeof<Migrations.Migration_20250106_001_InitialSetup>.Assembly)
                  .For.Migrations() |> ignore
            | _ ->
                failwith $"Unsupported database type: {databaseType}"

        services.AddFluentMigratorCore()
                .ConfigureRunner(Action<IMigrationRunnerBuilder> configureRunner)
                .AddLogging(fun lb -> lb.AddFluentMigratorConsole() |> ignore)
                .BuildServiceProvider(false)

    use scope = serviceProvider.CreateScope()
    let runner = scope.ServiceProvider.GetRequiredService<IMigrationRunner>()
    runner.MigrateUp()
