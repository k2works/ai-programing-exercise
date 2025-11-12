open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting

[<assembly: System.Runtime.CompilerServices.InternalsVisibleTo("SalesManagement.Api.Tests")>]
do ()

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    // サービスの登録
    builder.Services.AddControllers() |> ignore
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
