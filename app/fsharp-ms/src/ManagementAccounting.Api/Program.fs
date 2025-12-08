namespace ManagementAccounting.Api
#nowarn "20"

open System
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Configuration
open MassTransit
open ManagementAccounting.Application.Ports.In
open ManagementAccounting.Application.Ports.Out
open ManagementAccounting.Application.UseCases
open ManagementAccounting.Infrastructure.MigrationRunner
open ManagementAccounting.Infrastructure.Adapters.External
open ManagementAccounting.Infrastructure.Persistence.Repositories
open ManagementAccounting.Infrastructure.Messaging
open ManagementAccounting.Infrastructure.Web.Controllers

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
            builder.Configuration.GetConnectionString("ManagementAccounting")

        // 財務会計サービスのベース URL
        let financialAccountingBaseUrl =
            match builder.Configuration.["FinancialAccountingService:BaseUrl"] with
            | null -> "http://localhost:5115"
            | url -> url

        // RabbitMQ 設定を取得
        let rabbitMqHost = builder.Configuration.["RabbitMQ:Host"]

        // MassTransit 設定（Consumer 登録付き）
        builder.Services.AddMassTransit(fun (x: IBusRegistrationConfigurator) ->
            x.AddConsumer<JournalCreatedEventConsumer>() |> ignore
            x.AddConsumer<JournalUpdatedEventConsumer>() |> ignore
            x.AddConsumer<JournalDeletedEventConsumer>() |> ignore
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
                    cfg.ConfigureEndpoints(context)
                )
        ) |> ignore

        // HttpClient の登録（財務会計サービス用）
        builder.Services.AddHttpClient<FinancialAccountingClient>(fun client ->
            client.BaseAddress <- Uri(financialAccountingBaseUrl)
            client.DefaultRequestHeaders.Add("Accept", "application/json")
        ) |> ignore

        // DI 設定 - 腐敗防止層
        builder.Services.AddScoped<IFinancialDataPort>(fun sp ->
            sp.GetRequiredService<FinancialAccountingClient>() :> IFinancialDataPort)

        // DI 設定 - キャッシュリポジトリ
        builder.Services.AddScoped<IFinancialAnalysisCacheRepository>(fun _ ->
            FinancialAnalysisCacheRepository(connectionString) :> IFinancialAnalysisCacheRepository)

        // DI 設定 - イベントハンドラ
        builder.Services.AddScoped<IJournalEventHandler, JournalEventHandler>()

        // DI 設定 - ユースケース
        builder.Services.AddScoped<IFinancialAnalysisUseCase>(fun sp ->
            let financialDataPort = sp.GetRequiredService<IFinancialDataPort>()
            let cacheRepository = sp.GetRequiredService<IFinancialAnalysisCacheRepository>()
            FinancialAnalysisUseCase(financialDataPort, cacheRepository) :> IFinancialAnalysisUseCase)

        builder.Services.AddControllers()
            .AddApplicationPart(typeof<FinancialAnalysisController>.Assembly)
        |> ignore

        builder.Services.AddEndpointsApiExplorer()
        builder.Services.AddSwaggerGen()

        let app = builder.Build()

        // マイグレーション実行
        if not (String.IsNullOrEmpty(connectionString)) then
            migrateDatabase connectionString

        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI() |> ignore

        app.UseAuthorization()
        app.MapControllers()

        // ルートパスを Swagger UI にリダイレクト
        app.MapGet("/", Func<Microsoft.AspNetCore.Http.IResult>(fun () ->
            Microsoft.AspNetCore.Http.Results.Redirect("/swagger"))) |> ignore

        app.Run()
        0
