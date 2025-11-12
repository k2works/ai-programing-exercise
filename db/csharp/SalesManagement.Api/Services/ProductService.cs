using System.Data;
using Dapper;
using MySql.Data.MySqlClient;
using Npgsql;
using SalesManagement.Api.Dtos;
using SalesManagement.Api.Exceptions;
using SalesManagement.Domain.Models;
using SalesManagement.Infrastructure.Repositories;

namespace SalesManagement.Api.Services;

public class ProductService
{
    private readonly ProductRepository _productRepository;
    private readonly string _connectionString;
    private readonly string _databaseType;

    public ProductService(ProductRepository productRepository, IConfiguration configuration)
    {
        _productRepository = productRepository;
        _databaseType = configuration["DatabaseType"] ?? "PostgreSQL";
        _connectionString = configuration.GetConnectionString(_databaseType)
            ?? throw new InvalidOperationException($"接続文字列が設定されていません: {_databaseType}");
    }

    private IDbConnection CreateConnection()
    {
        return _databaseType switch
        {
            "MySQL" => new MySqlConnection(_connectionString),
            "PostgreSQL" => new NpgsqlConnection(_connectionString),
            _ => throw new InvalidOperationException($"未対応のデータベース: {_databaseType}")
        };
    }

    /// <summary>
    /// 商品を作成
    /// </summary>
    public async Task<ProductResponse> CreateProductAsync(CreateProductRequest request)
    {
        // ビジネスルール: 販売単価 >= 売上原価
        if (request.UnitPrice < request.PrimeCost)
        {
            throw new BusinessException("販売単価が売上原価より低い設定はできません");
        }

        // 商品コードの重複チェック
        var existing = await _productRepository.FindByIdAsync(request.ProductCode);
        if (existing != null)
        {
            throw new BusinessException($"商品コード {request.ProductCode} は既に存在します");
        }

        var product = new Product
        {
            ProductCode = request.ProductCode,
            ProductFormalName = request.FullName,
            ProductAbbreviation = request.Name,
            ProductNameKana = request.KanaName,
            SellingPrice = request.UnitPrice,
            CostOfSales = request.PrimeCost,
            SupplierCode = request.SupplierCode,
            ProductType = "PRODUCT", // デフォルト値
            ProductCategoryCode = "CAT001", // デフォルト値
            PurchasePrice = request.PrimeCost, // 仕入単価 = 売上原価
            TaxType = 1, // デフォルト値（課税）
            MiscellaneousType = 0, // デフォルト値（通常商品）
            InventoryManagementFlag = 1, // デフォルト値（在庫管理対象）
            InventoryAllocationFlag = 0, // デフォルト値（在庫引当しない）
            SupplierBranch = 0, // デフォルト値
            CreatedAt = DateTime.Now,
            CreatedBy = "SYSTEM", // 実際は認証ユーザーから取得
            UpdatedAt = DateTime.Now,
            UpdatedBy = "SYSTEM"
        };

        await _productRepository.InsertAsync(product);
        return ProductResponse.FromEntity(product);
    }

    /// <summary>
    /// すべての商品を取得
    /// </summary>
    public async Task<List<ProductResponse>> GetAllProductsAsync()
    {
        var products = await _productRepository.FindAllAsync();
        return products.Select(ProductResponse.FromEntity).ToList();
    }

    /// <summary>
    /// ページング対応の商品一覧取得
    /// </summary>
    public async Task<PageResponse<ProductResponse>> GetProductsAsync(int page, int size)
    {
        using var connection = CreateConnection();
        connection.Open();

        int offset = page * size;

        // ページングクエリ
        var sql = @"
            SELECT
                商品コード AS ProductCode,
                商品正式名 AS ProductFormalName,
                商品略称 AS ProductAbbreviation,
                商品名カナ AS ProductNameKana,
                商品区分 AS ProductType,
                製品型番 AS ModelNumber,
                販売単価 AS SellingPrice,
                仕入単価 AS PurchasePrice,
                売上原価 AS CostOfSales,
                税区分 AS TaxType,
                商品分類コード AS ProductCategoryCode,
                雑区分 AS MiscellaneousType,
                在庫管理対象区分 AS InventoryManagementFlag,
                在庫引当区分 AS InventoryAllocationFlag,
                仕入先コード AS SupplierCode,
                仕入先枝番 AS SupplierBranch,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 商品マスタ
            ORDER BY 商品コード
            LIMIT @Size OFFSET @Offset";

        var products = await connection.QueryAsync<Product>(sql, new { Size = size, Offset = offset });

        // 総件数取得
        var countSql = "SELECT COUNT(*) FROM 商品マスタ";
        var total = await connection.ExecuteScalarAsync<int>(countSql);

        var content = products.Select(ProductResponse.FromEntity).ToList();
        return new PageResponse<ProductResponse>(content, page, size, total);
    }

    /// <summary>
    /// IDで商品を取得
    /// </summary>
    public async Task<ProductResponse> GetProductByIdAsync(string productCode)
    {
        var product = await _productRepository.FindByIdAsync(productCode);
        if (product == null)
        {
            throw new ResourceNotFoundException($"商品コード {productCode} が見つかりません");
        }

        return ProductResponse.FromEntity(product);
    }

    /// <summary>
    /// 商品を更新
    /// </summary>
    public async Task<ProductResponse> UpdateProductAsync(string productCode, UpdateProductRequest request)
    {
        var existing = await _productRepository.FindByIdAsync(productCode);
        if (existing == null)
        {
            throw new ResourceNotFoundException($"商品コード {productCode} が見つかりません");
        }

        // ビジネスルール: 販売単価 >= 売上原価
        var newUnitPrice = request.UnitPrice ?? existing.SellingPrice;
        var newPrimeCost = request.PrimeCost ?? existing.CostOfSales;

        if (newUnitPrice < newPrimeCost)
        {
            throw new BusinessException("販売単価が売上原価より低い設定はできません");
        }

        // 更新項目の適用
        if (request.FullName != null) existing.ProductFormalName = request.FullName;
        if (request.Name != null) existing.ProductAbbreviation = request.Name;
        if (request.KanaName != null) existing.ProductNameKana = request.KanaName;
        if (request.UnitPrice.HasValue) existing.SellingPrice = request.UnitPrice.Value;
        if (request.PrimeCost.HasValue)
        {
            existing.CostOfSales = request.PrimeCost.Value;
            existing.PurchasePrice = request.PrimeCost.Value; // 仕入単価も更新
        }
        if (request.SupplierCode != null) existing.SupplierCode = request.SupplierCode;

        existing.UpdatedAt = DateTime.Now;
        existing.UpdatedBy = "SYSTEM";

        await _productRepository.UpdateAsync(existing);
        return ProductResponse.FromEntity(existing);
    }

    /// <summary>
    /// 商品を削除
    /// </summary>
    public async Task DeleteProductAsync(string productCode)
    {
        var existing = await _productRepository.FindByIdAsync(productCode);
        if (existing == null)
        {
            throw new ResourceNotFoundException($"商品コード {productCode} が見つかりません");
        }

        await _productRepository.DeleteAsync(productCode);
    }
}
