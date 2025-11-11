using SalesManagement.Api.Middleware;
using SalesManagement.Api.Services;
using SalesManagement.Infrastructure.Repositories;

var builder = WebApplication.CreateBuilder(args);

// サービスの登録
builder.Services.AddControllers();
builder.Services.AddEndpointsApiExplorer();
builder.Services.AddSwaggerGen(options =>
{
    options.SwaggerDoc("v1", new()
    {
        Title = "販売管理システムAPI",
        Version = "v1",
        Description = "Dapperを使用した販売管理システムのREST API"
    });

    // XMLコメントを有効化
    var xmlFile = $"{System.Reflection.Assembly.GetExecutingAssembly().GetName().Name}.xml";
    var xmlPath = Path.Combine(AppContext.BaseDirectory, xmlFile);
    if (File.Exists(xmlPath))
    {
        options.IncludeXmlComments(xmlPath);
    }
});

// Repository層の登録
builder.Services.AddScoped(sp =>
{
    var configuration = sp.GetRequiredService<IConfiguration>();
    var connectionString = configuration.GetConnectionString("DefaultConnection")
        ?? throw new InvalidOperationException("接続文字列が設定されていません");
    return new ProductRepository(connectionString);
});

// Service層の登録
builder.Services.AddScoped<ProductService>();

var app = builder.Build();

// ミドルウェアの設定
if (app.Environment.IsDevelopment())
{
    app.UseSwagger();
    app.UseSwaggerUI();
}

app.UseHttpsRedirection();

// グローバル例外ハンドラ
app.UseMiddleware<GlobalExceptionHandler>();

app.UseAuthorization();
app.MapControllers();

app.Run();

// テストから参照できるようにpartial classとして公開
public partial class Program { }
