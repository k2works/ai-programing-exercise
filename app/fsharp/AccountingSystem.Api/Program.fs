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
open AccountingSystem.Application.Repositories
open AccountingSystem.Application.Services
open AccountingSystem.Infrastructure.Adapters
open AccountingSystem.Infrastructure.Repositories.FinancialStatementRepository
open AccountingSystem.Infrastructure.Web.Controllers
open AccountingSystem.Api.Middleware

/// WebApplicationFactory 用のマーカー型
type Program() = class end

module Program =
    let exitCode = 0

    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)

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

        // Application Services の登録
        builder.Services.AddScoped<IAccountService, AccountService>()
        builder.Services.AddScoped<IJournalService, JournalService>()
        builder.Services.AddScoped<IFinancialStatementService, FinancialStatementService>()

        // グローバル例外ハンドラーの登録
        builder.Services.AddExceptionHandler<GlobalExceptionHandler>()
        builder.Services.AddProblemDetails()

        let app = builder.Build()

        // 例外ハンドリングミドルウェア
        app.UseExceptionHandler()

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
