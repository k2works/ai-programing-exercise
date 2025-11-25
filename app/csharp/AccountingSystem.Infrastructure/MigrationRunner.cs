using FluentMigrator.Runner;
using Microsoft.Extensions.DependencyInjection;

namespace AccountingSystem.Infrastructure
{
    public class MigrationRunner
    {
        public static void MigrateDatabase(string connectionString, string databaseType = "PostgreSQL")
        {
            var serviceProvider = CreateServices(connectionString, databaseType);

            using (var scope = serviceProvider.CreateScope())
            {
                UpdateDatabase(scope.ServiceProvider);
            }
        }

        private static IServiceProvider CreateServices(string connectionString, string databaseType)
        {
            var services = new ServiceCollection();

            if (databaseType == "PostgreSQL")
            {
                services.AddFluentMigratorCore()
                    .ConfigureRunner(rb => rb
                        .AddPostgres()
                        .WithGlobalConnectionString(connectionString)
                        .ScanIn(typeof(MigrationRunner).Assembly).For.Migrations())
                    .AddLogging(lb => lb.AddFluentMigratorConsole());
            }
            else if (databaseType == "MySQL")
            {
                services.AddFluentMigratorCore()
                    .ConfigureRunner(rb => rb
                        .AddMySql5()
                        .WithGlobalConnectionString(connectionString)
                        .ScanIn(typeof(MigrationRunner).Assembly).For.Migrations())
                    .AddLogging(lb => lb.AddFluentMigratorConsole());
            }

            return services.BuildServiceProvider(false);
        }

        private static void UpdateDatabase(IServiceProvider serviceProvider)
        {
            var runner = serviceProvider.GetRequiredService<IMigrationRunner>();
            runner.MigrateUp();
        }
    }
}
