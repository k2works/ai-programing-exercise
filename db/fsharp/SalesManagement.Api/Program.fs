open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open System.Text.Json

[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("SalesManagement.Api.Tests")>]
do ()

// テスト用の Program 型定義（public にする）
[<Class>]
type public Program() =
    class
    end

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    // JSON シリアライザのオプション設定（F# レコード型のサポート）
    builder.Services.AddControllers()
        .AddJsonOptions(fun options ->
            options.JsonSerializerOptions.Converters.Add(System.Text.Json.Serialization.JsonFSharpConverter())
        ) |> ignore
    builder.Services.AddEndpointsApiExplorer() |> ignore
    builder.Services.AddSwaggerGen() |> ignore

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
