using System.Reflection;
using Microsoft.OpenApi.Models;
using Swashbuckle.AspNetCore.SwaggerGen;

namespace ProductionManagement.Api.Configuration;

public static class OpenApiConfiguration
{
    public static void ConfigureSwagger(SwaggerGenOptions options)
    {
        options.SwaggerDoc("v1", new OpenApiInfo
        {
            Title = "生産管理システム API",
            Description = "TDD で育てる生産管理システムの API ドキュメント",
            Version = "1.0.0",
            Contact = new OpenApiContact
            {
                Name = "開発チーム",
                Email = "dev@example.com"
            },
            License = new OpenApiLicense
            {
                Name = "MIT License",
                Url = new Uri("https://opensource.org/licenses/MIT")
            }
        });

        // XML ドキュメントの読み込み
        var xmlFilename = $"{Assembly.GetExecutingAssembly().GetName().Name}.xml";
        var xmlPath = Path.Combine(AppContext.BaseDirectory, xmlFilename);
        if (File.Exists(xmlPath))
        {
            options.IncludeXmlComments(xmlPath);
        }

        // Infrastructure プロジェクトの XML ドキュメントも読み込む
        var infrastructureXmlPath = Path.Combine(AppContext.BaseDirectory, "ProductionManagement.Infrastructure.xml");
        if (File.Exists(infrastructureXmlPath))
        {
            options.IncludeXmlComments(infrastructureXmlPath);
        }

        options.TagActionsBy(api =>
        {
            if (api.GroupName != null)
            {
                return [api.GroupName];
            }

            var controllerName = api.ActionDescriptor.RouteValues["controller"];
            return [controllerName ?? "default"];
        });

        options.DocumentFilter<TagDescriptionsDocumentFilter>();

        // エラーレスポンスの型を追加
        options.OperationFilter<ProblemDetailsOperationFilter>();
    }
}

public class TagDescriptionsDocumentFilter : IDocumentFilter
{
    public void Apply(OpenApiDocument swaggerDoc, DocumentFilterContext context)
    {
        swaggerDoc.Tags =
        [
            new OpenApiTag { Name = "Root", Description = "ルート API（ヘルスチェック）" },
            new OpenApiTag { Name = "Item", Description = "品目マスタ API" },
            new OpenApiTag { Name = "Bom", Description = "BOM（部品構成表）API" },
            new OpenApiTag { Name = "Supplier", Description = "取引先マスタ API" },
            new OpenApiTag { Name = "Order", Description = "オーダ API" },
            new OpenApiTag { Name = "Inventory", Description = "在庫 API" },
            new OpenApiTag { Name = "WorkOrder", Description = "作業指示 API" },
            new OpenApiTag { Name = "PurchaseOrder", Description = "発注 API" },
            new OpenApiTag { Name = "Mrp", Description = "MRP API" }
        ];
    }
}

/// <summary>
/// ProblemDetails 型をエラーレスポンスに追加するフィルター
/// </summary>
public class ProblemDetailsOperationFilter : IOperationFilter
{
    public void Apply(OpenApiOperation operation, OperationFilterContext context)
    {
        var problemDetailsSchema = new OpenApiSchema
        {
            Reference = new OpenApiReference
            {
                Type = ReferenceType.Schema,
                Id = "ProblemDetails"
            }
        };

        // 400 Bad Request
        if (!operation.Responses.ContainsKey("400"))
        {
            operation.Responses.Add("400", new OpenApiResponse
            {
                Description = "リクエストが不正です",
                Content = new Dictionary<string, OpenApiMediaType>
                {
                    ["application/problem+json"] = new OpenApiMediaType
                    {
                        Schema = problemDetailsSchema
                    }
                }
            });
        }

        // 500 Internal Server Error
        if (!operation.Responses.ContainsKey("500"))
        {
            operation.Responses.Add("500", new OpenApiResponse
            {
                Description = "内部エラーが発生しました",
                Content = new Dictionary<string, OpenApiMediaType>
                {
                    ["application/problem+json"] = new OpenApiMediaType
                    {
                        Schema = problemDetailsSchema
                    }
                }
            });
        }
    }
}
