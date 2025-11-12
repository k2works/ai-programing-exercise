module SalesManagement.Api.Services.ProductService

open System
open System.Threading.Tasks
open Npgsql
open Dapper
open SalesManagement.Domain.Models
open SalesManagement.Api.Dtos
open SalesManagement.Infrastructure.Repositories.ProductRepository

/// サービスエラーの定義
type ServiceError =
    | NotFound of string
    | BusinessError of string
    | DatabaseError of string

/// 商品を作成
let createProductAsync (connectionString: string) (request: CreateProductRequest) : Task<Result<ProductResponse, ServiceError>> =
    task {
        try
            // 商品コードの重複チェック
            let! existing = findByIdAsync connectionString request.ProductCode
            match existing with
            | Some _ ->
                return Error (BusinessError $"商品コード {request.ProductCode} は既に存在します")
            | None ->
                let now = DateTime.Now
                let product : Product =
                    { ProductCode = request.ProductCode
                      ProductFormalName = request.ProductFormalName
                      ProductAbbreviation = request.ProductAbbreviation
                      ProductNameKana = request.ProductNameKana
                      ProductType = request.ProductType
                      ModelNumber = request.ModelNumber
                      SellingPrice = request.SellingPrice
                      PurchasePrice = request.PurchasePrice
                      CostOfSales = request.CostOfSales
                      TaxType = request.TaxType
                      ProductCategoryCode = request.ProductCategoryCode
                      MiscellaneousType = request.MiscellaneousType
                      InventoryManagementFlag = request.InventoryManagementFlag
                      InventoryAllocationFlag = request.InventoryAllocationFlag
                      SupplierCode = request.SupplierCode
                      SupplierBranch = request.SupplierBranch
                      CreatedAt = now
                      CreatedBy = "SYSTEM"
                      UpdatedAt = now
                      UpdatedBy = "SYSTEM" }

                do! insertAsync connectionString product
                return Ok (ProductResponse.fromEntity product)
        with
        | ex ->
            return Error (DatabaseError ex.Message)
    }

/// すべての商品を取得
let getAllProductsAsync (connectionString: string) : Task<Result<ProductResponse list, ServiceError>> =
    task {
        try
            let! products = findAllAsync connectionString
            return Ok (products |> Seq.toList |> List.map ProductResponse.fromEntity)
        with
        | ex ->
            return Error (DatabaseError ex.Message)
    }

/// ページング対応の商品一覧取得
let getProductsAsync (connectionString: string) (page: int) (size: int) : Task<Result<PageResponse<ProductResponse>, ServiceError>> =
    task {
        try
            use connection = new NpgsqlConnection(connectionString)
            do! connection.OpenAsync()

            let offset = page * size

            // ページングクエリ
            let sql = """
                SELECT
                    商品コード AS ProductCode,
                    商品正式名 AS ProductFormalName,
                    商品略称 AS ProductAbbreviation,
                    商品名カナ AS ProductNameKana,
                    商品区分 AS ProductType,
                    型番 AS ModelNumber,
                    販売単価 AS SellingPrice,
                    仕入単価 AS PurchasePrice,
                    売上原価 AS CostOfSales,
                    税区分 AS TaxType,
                    商品分類コード AS ProductCategoryCode,
                    雑区分 AS MiscellaneousType,
                    在庫管理対象フラグ AS InventoryManagementFlag,
                    在庫引当フラグ AS InventoryAllocationFlag,
                    仕入先コード AS SupplierCode,
                    仕入先枝番 AS SupplierBranch,
                    作成日時 AS CreatedAt,
                    作成者名 AS CreatedBy,
                    更新日時 AS UpdatedAt,
                    更新者名 AS UpdatedBy
                FROM 商品マスタ
                ORDER BY 商品コード
                LIMIT @Size OFFSET @Offset
            """

            let! products = connection.QueryAsync<Product>(sql, {| Size = size; Offset = offset |})

            // 総件数取得
            let countSql = "SELECT COUNT(*) FROM 商品マスタ"
            let! total = connection.ExecuteScalarAsync<int64>(countSql)

            let content = products |> Seq.toList |> List.map ProductResponse.fromEntity
            let response = PageResponse.create content page size total

            return Ok response
        with
        | ex ->
            return Error (DatabaseError ex.Message)
    }

/// IDで商品を取得
let getProductByIdAsync (connectionString: string) (productCode: string) : Task<Result<ProductResponse, ServiceError>> =
    task {
        try
            let! product = findByIdAsync connectionString productCode
            match product with
            | Some p ->
                return Ok (ProductResponse.fromEntity p)
            | None ->
                return Error (NotFound $"商品コード {productCode} が見つかりません")
        with
        | ex ->
            return Error (DatabaseError ex.Message)
    }

/// 商品を更新
let updateProductAsync (connectionString: string) (productCode: string) (request: UpdateProductRequest) : Task<Result<ProductResponse, ServiceError>> =
    task {
        try
            let! existing = findByIdAsync connectionString productCode
            match existing with
            | None ->
                return Error (NotFound $"商品コード {productCode} が見つかりません")
            | Some product ->
                // 更新項目の適用
                let updated : Product =
                    { product with
                        ProductFormalName = request.ProductFormalName |> Option.defaultValue product.ProductFormalName
                        ProductAbbreviation = request.ProductAbbreviation |> Option.defaultValue product.ProductAbbreviation
                        ProductNameKana = request.ProductNameKana |> Option.defaultValue product.ProductNameKana
                        ProductType = request.ProductType |> Option.defaultValue product.ProductType
                        ModelNumber = request.ModelNumber |> Option.orElse product.ModelNumber
                        SellingPrice = request.SellingPrice |> Option.defaultValue product.SellingPrice
                        PurchasePrice = request.PurchasePrice |> Option.defaultValue product.PurchasePrice
                        CostOfSales = request.CostOfSales |> Option.defaultValue product.CostOfSales
                        TaxType = request.TaxType |> Option.defaultValue product.TaxType
                        ProductCategoryCode = request.ProductCategoryCode |> Option.defaultValue product.ProductCategoryCode
                        MiscellaneousType = request.MiscellaneousType |> Option.defaultValue product.MiscellaneousType
                        InventoryManagementFlag = request.InventoryManagementFlag |> Option.defaultValue product.InventoryManagementFlag
                        InventoryAllocationFlag = request.InventoryAllocationFlag |> Option.defaultValue product.InventoryAllocationFlag
                        SupplierCode = request.SupplierCode |> Option.orElse product.SupplierCode
                        SupplierBranch = request.SupplierBranch |> Option.orElse product.SupplierBranch
                        UpdatedAt = DateTime.Now
                        UpdatedBy = "SYSTEM" }

                do! updateAsync connectionString updated
                return Ok (ProductResponse.fromEntity updated)
        with
        | ex ->
            return Error (DatabaseError ex.Message)
    }

/// 商品を削除
let deleteProductAsync (connectionString: string) (productCode: string) : Task<Result<unit, ServiceError>> =
    task {
        try
            let! existing = findByIdAsync connectionString productCode
            match existing with
            | None ->
                return Error (NotFound $"商品コード {productCode} が見つかりません")
            | Some _ ->
                do! deleteAsync connectionString productCode
                return Ok ()
        with
        | ex ->
            return Error (DatabaseError ex.Message)
    }
