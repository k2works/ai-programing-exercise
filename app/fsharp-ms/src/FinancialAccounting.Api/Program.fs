namespace FinancialAccounting.Api
#nowarn "20"

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Configuration
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Application.UseCases
open FinancialAccounting.Infrastructure.Persistence.Repositories
open FinancialAccounting.Infrastructure.MigrationRunner

module Program =
    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)

        // 接続文字列を取得
        let connectionString =
            builder.Configuration.GetConnectionString("FinancialAccounting")

        // DI 設定
        builder.Services.AddScoped<IJournalRepository>(fun _ ->
            JournalRepository(connectionString) :> IJournalRepository)

        builder.Services.AddScoped<IJournalUseCase>(fun sp ->
            let repository = sp.GetRequiredService<IJournalRepository>()
            JournalUseCase(repository) :> IJournalUseCase)

        builder.Services.AddControllers()
        builder.Services.AddEndpointsApiExplorer()
        builder.Services.AddSwaggerGen()

        let app = builder.Build()

        // マイグレーション実行
        if not (System.String.IsNullOrEmpty(connectionString)) then
            migrateDatabase connectionString

        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI() |> ignore

        app.UseAuthorization()
        app.MapControllers()

        app.Run()
        0
