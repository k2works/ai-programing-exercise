namespace ApiGateway
#nowarn "20"

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Configuration
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Ocelot.DependencyInjection
open Ocelot.Middleware

module Program =
    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)

        // 環境名を取得
        let env = builder.Environment.EnvironmentName

        // Ocelot 設定ファイルの読み込み（環境別ファイルを優先）
        builder.Configuration
            .AddJsonFile("ocelot.json", optional = false, reloadOnChange = true)
            .AddJsonFile($"ocelot.{env}.json", optional = true, reloadOnChange = true)
        |> ignore

        // Ocelot サービスの登録
        builder.Services.AddOcelot(builder.Configuration) |> ignore

        // CORS 設定（開発時用）
        builder.Services.AddCors(fun options ->
            options.AddDefaultPolicy(fun policy ->
                policy.AllowAnyOrigin()
                      .AllowAnyHeader()
                      .AllowAnyMethod()
                |> ignore
            )
        ) |> ignore

        let app = builder.Build()

        // 開発環境でのみ詳細なエラー情報を表示
        if app.Environment.IsDevelopment() then
            app.UseDeveloperExceptionPage() |> ignore

        app.UseCors()

        // Ocelot ミドルウェアの使用
        app.UseOcelot().Wait()

        app.Run()
        0
