module SalesManagement.Infrastructure.Repositories.ProductCategoryRepository

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open SalesManagement.Domain.Models

/// <summary>
/// 商品分類を登録
/// </summary>
let insertAsync (connectionString: string) (category: ProductCategory) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 商品分類マスタ (
                商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @ProductCategoryCode, @ProductCategoryName, @ProductCategoryLevel,
                @ProductCategoryPath, @LowestLevelFlag,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, category)
        return ()
    }

/// <summary>
/// 商品分類を更新
/// </summary>
let updateAsync (connectionString: string) (category: ProductCategory) : Task<unit> =
    task {
        let sql = """
            UPDATE 商品分類マスタ
            SET 商品分類名 = @ProductCategoryName,
                商品分類階層 = @ProductCategoryLevel,
                商品分類パス = @ProductCategoryPath,
                最下層区分 = @LowestLevelFlag,
                更新日時 = @UpdatedAt,
                更新者名 = @UpdatedBy
            WHERE 商品分類コード = @ProductCategoryCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, category)
        return ()
    }

/// <summary>
/// 商品分類を削除
/// </summary>
let deleteAsync (connectionString: string) (productCategoryCode: string) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 商品分類マスタ
            WHERE 商品分類コード = @ProductCategoryCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| ProductCategoryCode = productCategoryCode |})
        return ()
    }

/// <summary>
/// 商品分類コードで検索
/// </summary>
let findByIdAsync (connectionString: string) (productCategoryCode: string) : Task<ProductCategory option> =
    task {
        let sql = """
            SELECT
                商品分類コード AS ProductCategoryCode,
                商品分類名 AS ProductCategoryName,
                商品分類階層 AS ProductCategoryLevel,
                商品分類パス AS ProductCategoryPath,
                最下層区分 AS LowestLevelFlag,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 商品分類マスタ
            WHERE 商品分類コード = @ProductCategoryCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<ProductCategory>(sql, {| ProductCategoryCode = productCategoryCode |})
        return if isNull (box result) then None else Some result
    }

/// <summary>
/// すべての商品分類を取得
/// </summary>
let findAllAsync (connectionString: string) : Task<seq<ProductCategory>> =
    task {
        let sql = """
            SELECT
                商品分類コード AS ProductCategoryCode,
                商品分類名 AS ProductCategoryName,
                商品分類階層 AS ProductCategoryLevel,
                商品分類パス AS ProductCategoryPath,
                最下層区分 AS LowestLevelFlag,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 商品分類マスタ
            ORDER BY 商品分類階層, 商品分類コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<ProductCategory>(sql)
        return results :> seq<ProductCategory>
    }

/// <summary>
/// 階層パスの前方一致で検索（配下の分類を取得）
/// </summary>
let findByPathPrefixAsync (connectionString: string) (pathPrefix: string) : Task<seq<ProductCategory>> =
    task {
        let sql = """
            SELECT
                商品分類コード AS ProductCategoryCode,
                商品分類名 AS ProductCategoryName,
                商品分類階層 AS ProductCategoryLevel,
                商品分類パス AS ProductCategoryPath,
                最下層区分 AS LowestLevelFlag,
                作成日時 AS CreatedAt,
                作成者名 AS CreatedBy,
                更新日時 AS UpdatedAt,
                更新者名 AS UpdatedBy
            FROM 商品分類マスタ
            WHERE 商品分類パス LIKE @PathPrefix || '%'
            ORDER BY 商品分類階層, 商品分類コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<ProductCategory>(sql, {| PathPrefix = pathPrefix |})
        return results :> seq<ProductCategory>
    }
