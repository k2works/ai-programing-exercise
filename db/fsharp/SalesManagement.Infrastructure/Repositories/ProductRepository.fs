module SalesManagement.Infrastructure.Repositories.ProductRepository

open System
open System.Threading.Tasks
open Dapper
open Npgsql
open SalesManagement.Domain.Models

/// <summary>
/// 商品を登録
/// </summary>
let insertAsync (connectionString: string) (product: Product) : Task<unit> =
    task {
        let sql = """
            INSERT INTO 商品マスタ (
                商品コード, 商品正式名, 商品略称, 商品名カナ, 商品区分, 製品型番,
                販売単価, 仕入単価, 売上原価, 税区分, 商品分類コード,
                雑区分, 在庫管理対象区分, 在庫引当区分, 仕入先コード, 仕入先枝番,
                作成日時, 作成者名, 更新日時, 更新者名
            ) VALUES (
                @ProductCode, @ProductFormalName, @ProductAbbreviation, @ProductNameKana,
                @ProductType, @ModelNumber, @SellingPrice, @PurchasePrice, @CostOfSales,
                @TaxType, @ProductCategoryCode, @MiscellaneousType, @InventoryManagementFlag,
                @InventoryAllocationFlag, @SupplierCode, @SupplierBranch,
                @CreatedAt, @CreatedBy, @UpdatedAt, @UpdatedBy
            )"""

        use connection = new NpgsqlConnection(connectionString)
        // F# の option 型を null 許容型に変換
        let parameters = {|
            ProductCode = product.ProductCode
            ProductFormalName = product.ProductFormalName
            ProductAbbreviation = product.ProductAbbreviation
            ProductNameKana = product.ProductNameKana
            ProductType = product.ProductType
            ModelNumber = product.ModelNumber |> Option.toObj
            SellingPrice = product.SellingPrice
            PurchasePrice = product.PurchasePrice
            CostOfSales = product.CostOfSales
            TaxType = product.TaxType
            ProductCategoryCode = product.ProductCategoryCode
            MiscellaneousType = product.MiscellaneousType
            InventoryManagementFlag = product.InventoryManagementFlag
            InventoryAllocationFlag = product.InventoryAllocationFlag
            SupplierCode = product.SupplierCode |> Option.toObj
            SupplierBranch = product.SupplierBranch |> Option.toNullable
            CreatedAt = product.CreatedAt
            CreatedBy = product.CreatedBy
            UpdatedAt = product.UpdatedAt
            UpdatedBy = product.UpdatedBy
        |}
        let! _ = connection.ExecuteAsync(sql, parameters)
        return ()
    }

/// <summary>
/// 商品を更新
/// </summary>
let updateAsync (connectionString: string) (product: Product) : Task<unit> =
    task {
        let sql = """
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
            WHERE 商品コード = @ProductCode"""

        use connection = new NpgsqlConnection(connectionString)
        // F# の option 型を null 許容型に変換
        let parameters = {|
            ProductCode = product.ProductCode
            ProductFormalName = product.ProductFormalName
            ProductAbbreviation = product.ProductAbbreviation
            ProductNameKana = product.ProductNameKana
            ProductType = product.ProductType
            ModelNumber = product.ModelNumber |> Option.toObj
            SellingPrice = product.SellingPrice
            PurchasePrice = product.PurchasePrice
            CostOfSales = product.CostOfSales
            TaxType = product.TaxType
            ProductCategoryCode = product.ProductCategoryCode
            MiscellaneousType = product.MiscellaneousType
            InventoryManagementFlag = product.InventoryManagementFlag
            InventoryAllocationFlag = product.InventoryAllocationFlag
            SupplierCode = product.SupplierCode |> Option.toObj
            SupplierBranch = product.SupplierBranch |> Option.toNullable
            UpdatedAt = product.UpdatedAt
            UpdatedBy = product.UpdatedBy
        |}
        let! _ = connection.ExecuteAsync(sql, parameters)
        return ()
    }

/// <summary>
/// 商品を削除
/// </summary>
let deleteAsync (connectionString: string) (productCode: string) : Task<unit> =
    task {
        let sql = """
            DELETE FROM 商品マスタ
            WHERE 商品コード = @ProductCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! _ = connection.ExecuteAsync(sql, {| ProductCode = productCode |})
        return ()
    }

/// <summary>
/// 商品コードで検索
/// </summary>
let findByIdAsync (connectionString: string) (productCode: string) : Task<Product option> =
    task {
        let sql = """
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
            WHERE 商品コード = @ProductCode"""

        use connection = new NpgsqlConnection(connectionString)
        let! result = connection.QuerySingleOrDefaultAsync<Product>(sql, {| ProductCode = productCode |})
        return if isNull (box result) then None else Some result
    }

/// <summary>
/// すべての商品を取得
/// </summary>
let findAllAsync (connectionString: string) : Task<seq<Product>> =
    task {
        let sql = """
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
            ORDER BY 商品コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Product>(sql)
        return results :> seq<Product>
    }

/// <summary>
/// 商品分類コードで検索
/// </summary>
let findByCategoryAsync (connectionString: string) (productCategoryCode: string) : Task<seq<Product>> =
    task {
        let sql = """
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
            ORDER BY 商品コード"""

        use connection = new NpgsqlConnection(connectionString)
        let! results = connection.QueryAsync<Product>(sql, {| ProductCategoryCode = productCategoryCode |})
        return results :> seq<Product>
    }
