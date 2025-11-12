using System.Data;
using Dapper;
using MySql.Data.MySqlClient;
using Npgsql;
using SalesManagement.Domain.Models;

namespace SalesManagement.Infrastructure.Repositories
{
    /// <summary>
    /// 商品マスタのRepositoryクラス
    /// </summary>
    public class ProductRepository
    {
        private readonly string _connectionString;
        private readonly string _databaseType;

        public ProductRepository(string connectionString, string databaseType = "PostgreSQL")
        {
            _connectionString = connectionString;
            _databaseType = databaseType;
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
        /// 商品を登録
        /// </summary>
        public async Task InsertAsync(Product product)
        {
            const string sql = @"
                INSERT INTO 商品マスタ (
                    商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
                    販売単価, 仕入単価, 売上原価, 税区分, 商品分類コード,
                    雑区分, 在庫管理対象区分, 在庫引当区分, 仕入先コード, 仕入先枝番,
                    作成日時, 作成者名, 更新日時, 更新者名
                ) VALUES (
                    @ProductCode, @ProductFormalName, @ProductAbbreviation, @ProductNameKana,
                    @ProductType, @ModelNumber, @SellingPrice, @PurchasePrice, @CostOfSales,
                    @TaxType, @ProductCategoryCode, @MiscellaneousType,
                    @InventoryManagementFlag, @InventoryAllocationFlag,
                    @SupplierCode, @SupplierBranch,
                    @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
                )";

            using var connection = CreateConnection();
            await connection.ExecuteAsync(sql, product);
        }

        /// <summary>
        /// 商品を更新
        /// </summary>
        public async Task UpdateAsync(Product product)
        {
            const string sql = @"
                UPDATE 商品マスタ
                SET 商品正式名 = @ProductFormalName,
                    商品略称 = @ProductAbbreviation,
                    商品名カナ = @ProductNameKana,
                    商品区分 = @ProductType,
                    製品型番 = @ModelNumber,
                    販売単価 = @SellingPrice,
                    仕入単価 = @PurchasePrice,
                    売上原価 = @CostOfSales,
                    税区分 = @TaxType,
                    商品分類コード = @ProductCategoryCode,
                    雑区分 = @MiscellaneousType,
                    在庫管理対象区分 = @InventoryManagementFlag,
                    在庫引当区分 = @InventoryAllocationFlag,
                    仕入先コード = @SupplierCode,
                    仕入先枝番 = @SupplierBranch,
                    更新日時 = @UpdatedAt,
                    更新者名 = @UpdatedBy
                WHERE 商品コード = @ProductCode";

            using var connection = CreateConnection();
            await connection.ExecuteAsync(sql, product);
        }

        /// <summary>
        /// 商品を削除
        /// </summary>
        public async Task DeleteAsync(string productCode)
        {
            const string sql = @"
                DELETE FROM 商品マスタ
                WHERE 商品コード = @ProductCode";

            using var connection = CreateConnection();
            await connection.ExecuteAsync(sql, new { ProductCode = productCode });
        }

        /// <summary>
        /// 商品コードで検索
        /// </summary>
        public async Task<Product?> FindByIdAsync(string productCode)
        {
            const string sql = @"
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
                WHERE 商品コード = @ProductCode";

            using var connection = CreateConnection();
            return await connection.QuerySingleOrDefaultAsync<Product>(sql, new { ProductCode = productCode });
        }

        /// <summary>
        /// すべての商品を取得
        /// </summary>
        public async Task<IEnumerable<Product>> FindAllAsync()
        {
            const string sql = @"
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
                ORDER BY 商品コード";

            using var connection = CreateConnection();
            return await connection.QueryAsync<Product>(sql);
        }

        /// <summary>
        /// 商品分類コードで商品を検索
        /// </summary>
        public async Task<IEnumerable<Product>> FindByCategoryAsync(string productCategoryCode)
        {
            const string sql = @"
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
                WHERE 商品分類コード = @ProductCategoryCode
                ORDER BY 商品コード";

            using var connection = CreateConnection();
            return await connection.QueryAsync<Product>(sql, new { ProductCategoryCode = productCategoryCode });
        }
    }
}
