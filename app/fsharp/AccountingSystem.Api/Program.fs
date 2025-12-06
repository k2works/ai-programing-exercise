namespace AccountingSystem.Api

#nowarn "20"
open System
open System.IO
open System.Reflection
open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Configuration
open Microsoft.OpenApi.Models
open AccountingSystem.Application.Port.In
open AccountingSystem.Application.Port.Out
open AccountingSystem.Application.Services
open AccountingSystem.Application.EventHandlers
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.Messaging
open AccountingSystem.Infrastructure.Persistence.Repositories.FinancialStatementRepository
open AccountingSystem.Infrastructure.Web.Controllers
open AccountingSystem.Infrastructure.Seed
open AccountingSystem.Api.Middleware

/// WebApplicationFactory 用のマーカー型
type Program() = class end

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)

        // CORS の設定
        builder.Services.AddCors(fun options ->
            options.AddPolicy("AllowAll", fun policy ->
                policy
                    .AllowAnyOrigin()
                    .AllowAnyMethod()
                    .AllowAnyHeader()
                |> ignore
            )
        ) |> ignore

        // Add services to the container.
        // コントローラーが Infrastructure アセンブリに移動したため、明示的にアセンブリを登録
        builder.Services
            .AddControllers()
            .AddApplicationPart(typeof<AccountController>.Assembly)
        |> ignore

        // Swagger/OpenAPI の設定
        builder.Services.AddEndpointsApiExplorer()
        builder.Services.AddSwaggerGen(fun options ->
            options.SwaggerDoc("v1", OpenApiInfo(
                Title = "財務会計システム API",
                Version = "v1",
                Description = "財務会計システムの REST API ドキュメント",
                Contact = OpenApiContact(
                    Name = "開発チーム",
                    Email = "dev@example.com"
                )
            ))

            // XML コメントを有効化
            let xmlFile = $"{Assembly.GetExecutingAssembly().GetName().Name}.xml"
            let xmlPath = Path.Combine(AppContext.BaseDirectory, xmlFile)
            if File.Exists(xmlPath) then
                options.IncludeXmlComments(xmlPath)
        )

        // データベース接続文字列を取得
        let connectionString =
            builder.Configuration.GetConnectionString("DefaultConnection")
            |> Option.ofObj
            |> Option.defaultValue "Host=localhost;Port=5432;Database=accounting_system;Username=postgres;Password=postgres"

        // RabbitMQ 設定を取得
        let rabbitMqSection = builder.Configuration.GetSection("RabbitMQ")
        let rabbitMqEnabled =
            rabbitMqSection.GetValue<bool>("Enabled", false)
        let rabbitMqConfig : RabbitMqConfig = {
            HostName = rabbitMqSection.GetValue<string>("HostName", "localhost")
            Port = rabbitMqSection.GetValue<int>("Port", 5672)
            UserName = rabbitMqSection.GetValue<string>("UserName", "guest")
            Password = rabbitMqSection.GetValue<string>("Password", "guest")
            VirtualHost = rabbitMqSection.GetValue<string>("VirtualHost", "/")
            ExchangeName = rabbitMqSection.GetValue<string>("ExchangeName", "accounting.events")
        }

        // Repository の登録（Output Adapters）
        builder.Services.AddScoped<IAccountRepository>(fun _ ->
            AccountRepositoryAdapter(connectionString) :> IAccountRepository
        )

        builder.Services.AddScoped<IJournalRepository>(fun _ ->
            JournalRepositoryAdapter(connectionString) :> IJournalRepository
        )

        builder.Services.AddScoped<IFinancialStatementRepository>(fun _ ->
            FinancialStatementRepositoryAdapter(connectionString) :> IFinancialStatementRepository
        )

        builder.Services.AddScoped<IAuditLogRepository>(fun _ ->
            AuditLogRepositoryAdapter(connectionString) :> IAuditLogRepository
        )

        builder.Services.AddScoped<IEventStoreRepository>(fun _ ->
            EventStoreRepositoryAdapter(connectionString) :> IEventStoreRepository
        )

        builder.Services.AddScoped<ISnapshotRepository>(fun _ ->
            SnapshotRepositoryAdapter(connectionString) :> ISnapshotRepository
        )

        builder.Services.AddScoped<IJournalEntryReadModelRepository>(fun _ ->
            JournalEntryReadModelRepositoryAdapter(connectionString) :> IJournalEntryReadModelRepository
        )

        // Application Services の登録
        builder.Services.AddScoped<IAccountUseCase, AccountService>()
        builder.Services.AddScoped<IJournalUseCase, JournalService>()
        builder.Services.AddScoped<IFinancialStatementUseCase, FinancialStatementService>()
        builder.Services.AddScoped<IFinancialAnalysisUseCase, FinancialAnalysisService>()
        builder.Services.AddScoped<IAuditLogUseCase, AuditLogService>()
        builder.Services.AddScoped<IJournalEntryEventSourcingUseCase, JournalEntryEventSourcingService>()

        // スナップショット最適化版サービスの登録
        builder.Services.AddScoped<JournalEntryEventSourcingServiceWithSnapshot>(fun sp ->
            let eventStoreRepo = sp.GetRequiredService<IEventStoreRepository>()
            let snapshotRepo = sp.GetRequiredService<ISnapshotRepository>()
            JournalEntryEventSourcingServiceWithSnapshot(eventStoreRepo, snapshotRepo, 10)
        )

        // イベントハンドラーの登録
        builder.Services.AddScoped<IJournalEntryEventHandler>(fun sp ->
            let readModelRepo = sp.GetRequiredService<IJournalEntryReadModelRepository>()
            JournalEntryReadModelHandler(readModelRepo)
        )
        |> ignore

        builder.Services.AddScoped<IJournalEntryEventHandler>(fun sp ->
            let auditLogRepo = sp.GetRequiredService<IAuditLogRepository>()
            AuditLogEventHandler(auditLogRepo)
        )
        |> ignore

        // イベントディスパッチャー（ローカル用）の登録
        builder.Services.AddScoped<EventDispatcher>(fun sp ->
            let handlers = sp.GetServices<IJournalEntryEventHandler>()
            EventDispatcher(handlers)
        )

        // イベントパブリッシャー（IEventPublisher）の登録
        // RabbitMQ が有効な場合は RabbitMqEventPublisher、無効な場合は EventDispatcher を使用
        if rabbitMqEnabled then
            builder.Services.AddSingleton<RabbitMqEventPublisher>(fun _ ->
                new RabbitMqEventPublisher(rabbitMqConfig)
            )
            |> ignore
            builder.Services.AddScoped<IEventPublisher>(fun sp ->
                sp.GetRequiredService<RabbitMqEventPublisher>() :> IEventPublisher
            )
            |> ignore

            // RabbitMQ イベントサブスクライバーを登録
            builder.Services.AddHostedService<RabbitMqEventSubscriber>(fun sp ->
                let scopeFactory = sp.GetRequiredService<IServiceScopeFactory>()
                let logger = sp.GetRequiredService<Microsoft.Extensions.Logging.ILogger<RabbitMqEventSubscriber>>()
                new RabbitMqEventSubscriber(rabbitMqConfig, scopeFactory, logger)
            )
            |> ignore
        else
            builder.Services.AddScoped<IEventPublisher>(fun sp ->
                sp.GetRequiredService<EventDispatcher>() :> IEventPublisher
            )
            |> ignore

        // イベント発行付きイベントソーシングサービスの登録
        builder.Services.AddScoped<JournalEntryEventSourcingServiceWithEvents>(fun sp ->
            let eventStoreRepo = sp.GetRequiredService<IEventStoreRepository>()
            let snapshotRepo = sp.GetRequiredService<ISnapshotRepository>()
            let eventPublisher = sp.GetRequiredService<IEventPublisher>()
            JournalEntryEventSourcingServiceWithEvents(eventStoreRepo, snapshotRepo, eventPublisher, 10)
        )

        // グローバル例外ハンドラーの登録
        builder.Services.AddExceptionHandler<GlobalExceptionHandler>()
        builder.Services.AddProblemDetails()

        // DatabaseSeeder の登録（IHostedService として起動時に Seed を実行）
        builder.Services.AddHostedService<DatabaseSeeder>(fun sp ->
            let logger = sp.GetRequiredService<Microsoft.Extensions.Logging.ILogger<DatabaseSeeder>>()
            DatabaseSeeder(connectionString, logger)
        )
        |> ignore

        let app = builder.Build()

        // 例外ハンドリングミドルウェア
        app.UseExceptionHandler()

        // CORS ミドルウェア
        app.UseCors("AllowAll") |> ignore

        // Configure the HTTP request pipeline.
        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI(fun options ->
                options.SwaggerEndpoint("/swagger/v1/swagger.json", "財務会計システム API v1")
            ) |> ignore

        app.UseHttpsRedirection()
        app.UseAuthorization()
        app.MapControllers()

        app.Run()

        exitCode
