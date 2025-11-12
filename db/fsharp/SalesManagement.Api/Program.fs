namespace SalesManagement.Api

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

// WebApplicationFactory から参照可能にするためのダミークラス
type Program() = class end

module Main =
    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder(args)

        // JSON シリアライザのオプション設定（F# レコード型のサポート）
        builder.Services.AddControllers()
            .AddJsonOptions(fun options ->
                options.JsonSerializerOptions.Converters.Add(System.Text.Json.Serialization.JsonFSharpConverter())
            ) |> ignore
        builder.Services.AddEndpointsApiExplorer() |> ignore
        builder.Services.AddSwaggerGen(fun c ->
            // Swagger Annotations を有効化
            c.EnableAnnotations()
            // F# の option 型のスキーマを展開
            c.UseAllOfToExtendReferenceSchemas()
        ) |> ignore

        let app = builder.Build()

        // ミドルウェアの設定
        if app.Environment.IsDevelopment() then
            app.UseSwagger() |> ignore
            app.UseSwaggerUI() |> ignore

        app.UseHttpsRedirection() |> ignore
        app.UseAuthorization() |> ignore
        app.MapControllers() |> ignore

        app.Run()

        0
