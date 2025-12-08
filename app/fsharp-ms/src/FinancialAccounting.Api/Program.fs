namespace FinancialAccounting.Api
#nowarn "20"

open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Configuration
open MassTransit
open FinancialAccounting.Application.Ports.In
open FinancialAccounting.Application.Ports.Out
open FinancialAccounting.Application.UseCases
open FinancialAccounting.Infrastructure.Persistence.Repositories
open FinancialAccounting.Infrastructure.Messaging
open FinancialAccounting.Infrastructure.DependencyInjection
open FinancialAccounting.Infrastructure.MigrationRunner
open FinancialAccounting.Infrastructure.Web.Controllers

/// <summary>
/// WebApplicationFactory 用のマーカー型
/// </summary>
type IApiMarker = interface end

module Program =
    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)

        // 接続文字列を取得
        let connectionString =
            builder.Configuration.GetConnectionString("FinancialAccounting")

        // RabbitMQ 設定を取得
        let rabbitMqHost = builder.Configuration.["RabbitMQ:Host"]

        // MassTransit 設定（RabbitMQ が設定されている場合のみ使用、そうでなければインメモリ）
        builder.Services.AddMassTransit(fun (x: IBusRegistrationConfigurator) ->
            match rabbitMqHost with
            | null | "" ->
                // テスト環境: インメモリトランスポート使用
                x.UsingInMemory(fun (context: IBusRegistrationContext) (cfg: IInMemoryBusFactoryConfigurator) ->
                    cfg.ConfigureEndpoints(context)
                )
            | host ->
                // 本番環境: RabbitMQ 使用
                let rabbitMqUsername =
                    match builder.Configuration.["RabbitMQ:Username"] with
                    | null -> "guest"
                    | username -> username
                let rabbitMqPassword =
                    match builder.Configuration.["RabbitMQ:Password"] with
                    | null -> "guest"
                    | password -> password
                x.UsingRabbitMq(fun (context: IBusRegistrationContext) (cfg: IRabbitMqBusFactoryConfigurator) ->
                    cfg.Host(host, fun (h: IRabbitMqHostConfigurator) ->
                        h.Username(rabbitMqUsername)
                        h.Password(rabbitMqPassword)
                    )
                )
        ) |> ignore

        // DI 設定 - EventPublisher
        builder.Services.AddScoped<IJournalEventPublisher, JournalEventPublisher>()

        // DI 設定 - Journal
        builder.Services.AddScoped<IJournalRepository>(fun _ ->
            JournalRepository(connectionString) :> IJournalRepository)

        builder.Services.AddScoped<IJournalUseCase>(fun sp ->
            let repository = sp.GetRequiredService<IJournalRepository>()
            let eventPublisher = sp.GetRequiredService<IJournalEventPublisher>()
            JournalUseCase(repository, eventPublisher) :> IJournalUseCase)

        // DI 設定 - Account
        builder.Services.AddScoped<IAccountRepository>(fun _ ->
            AccountRepository(connectionString) :> IAccountRepository)

        builder.Services.AddScoped<IAccountUseCase>(fun sp ->
            let repository = sp.GetRequiredService<IAccountRepository>()
            AccountUseCase(repository) :> IAccountUseCase)

        builder.Services.AddControllers()
            .AddApplicationPart(typeof<JournalController>.Assembly)
        |> ignore

        // CORS 設定（開発環境用）
        builder.Services.AddCors(fun options ->
            options.AddDefaultPolicy(fun policy ->
                policy.AllowAnyOrigin()
                      .AllowAnyMethod()
                      .AllowAnyHeader() |> ignore
            )
        ) |> ignore

        builder.Services.AddEndpointsApiExplorer()
        builder.Services.AddSwaggerGen()

        let app = builder.Build()

        // マイグレーション実行
        if not (String.IsNullOrEmpty(connectionString)) then
            migrateDatabase connectionString

        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI() |> ignore

        // CORS ミドルウェアを有効化
        app.UseCors() |> ignore

        app.UseAuthorization()
        app.MapControllers()

        // ルートパスを Swagger UI にリダイレクト
        app.MapGet("/", Func<Microsoft.AspNetCore.Http.IResult>(fun () ->
            Microsoft.AspNetCore.Http.Results.Redirect("/swagger"))) |> ignore

        app.Run()
        0
