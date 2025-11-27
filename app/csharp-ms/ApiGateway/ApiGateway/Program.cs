var builder = WebApplication.CreateBuilder(args);

// YARP Reverse Proxy の設定を読み込み
builder.Services.AddReverseProxy()
    .LoadFromConfig(builder.Configuration.GetSection("ReverseProxy"));

// CORS
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowAll", policy =>
    {
        policy.AllowAnyOrigin()
              .AllowAnyMethod()
              .AllowAnyHeader();
    });
});

var app = builder.Build();

// Configure the HTTP request pipeline
app.UseCors("AllowAll");

// YARP Reverse Proxy を有効化
app.MapReverseProxy();

// ヘルスチェックエンドポイント
app.MapGet("/health", () => Results.Ok(new { status = "healthy" }));

app.Run();

// テストで WebApplicationFactory を使用するために public に公開
public partial class Program { }
