namespace SalesManagement.Api.Dtos

open System
open System.ComponentModel.DataAnnotations
open SalesManagement.Domain.Models

/// 商品作成リクエストDTO
type CreateProductRequest =
    { [<Required(ErrorMessage = "商品コードは必須です")>]
      [<StringLength(16, ErrorMessage = "商品コードは16文字以内で入力してください")>]
      ProductCode: string

      [<Required(ErrorMessage = "商品正式名は必須です")>]
      [<StringLength(100, ErrorMessage = "商品正式名は100文字以内で入力してください")>]
      ProductFormalName: string

      [<Required(ErrorMessage = "商品略称は必須です")>]
      [<StringLength(40, ErrorMessage = "商品略称は40文字以内で入力してください")>]
      ProductAbbreviation: string

      [<Required(ErrorMessage = "商品名カナは必須です")>]
      [<StringLength(100, ErrorMessage = "商品名カナは100文字以内で入力してください")>]
      ProductNameKana: string

      [<Required(ErrorMessage = "商品区分は必須です")>]
      [<StringLength(8, ErrorMessage = "商品区分は8文字以内で入力してください")>]
      ProductType: string

      ModelNumber: string option

      [<Required(ErrorMessage = "販売単価は必須です")>]
      [<Range(0, Int32.MaxValue, ErrorMessage = "販売単価は0以上で入力してください")>]
      SellingPrice: int

      [<Required(ErrorMessage = "仕入単価は必須です")>]
      [<Range(0, Int32.MaxValue, ErrorMessage = "仕入単価は0以上で入力してください")>]
      PurchasePrice: int

      [<Required(ErrorMessage = "売上原価は必須です")>]
      [<Range(0, Int32.MaxValue, ErrorMessage = "売上原価は0以上で入力してください")>]
      CostOfSales: int

      [<Required(ErrorMessage = "税区分は必須です")>]
      [<Range(0, 2, ErrorMessage = "税区分は0-2の範囲で入力してください")>]
      TaxType: int

      [<Required(ErrorMessage = "商品分類コードは必須です")>]
      [<StringLength(8, ErrorMessage = "商品分類コードは8文字以内で入力してください")>]
      ProductCategoryCode: string

      [<Required(ErrorMessage = "雑区分は必須です")>]
      [<Range(0, 1, ErrorMessage = "雑区分は0-1の範囲で入力してください")>]
      MiscellaneousType: int

      [<Required(ErrorMessage = "在庫管理フラグは必須です")>]
      [<Range(0, 1, ErrorMessage = "在庫管理フラグは0-1の範囲で入力してください")>]
      InventoryManagementFlag: int

      [<Required(ErrorMessage = "在庫引当フラグは必須です")>]
      [<Range(0, 1, ErrorMessage = "在庫引当フラグは0-1の範囲で入力してください")>]
      InventoryAllocationFlag: int

      SupplierCode: string option

      SupplierBranch: int option }

/// 商品更新リクエストDTO（すべてのフィールドはオプション）
/// Note: option 型のフィールドにデータアノテーション属性を使用すると、
/// バリデーション時に InvalidCastException が発生するため、属性を削除しています。
/// バリデーションはサービス層で行います。
type UpdateProductRequest =
    { ProductFormalName: string option
      ProductAbbreviation: string option
      ProductNameKana: string option
      ProductType: string option
      ModelNumber: string option
      SellingPrice: int option
      PurchasePrice: int option
      CostOfSales: int option
      TaxType: int option
      ProductCategoryCode: string option
      MiscellaneousType: int option
      InventoryManagementFlag: int option
      InventoryAllocationFlag: int option
      SupplierCode: string option
      SupplierBranch: int option }

/// 商品レスポンスDTO
type ProductResponse =
    { ProductCode: string
      ProductFormalName: string
      ProductAbbreviation: string
      ProductNameKana: string
      ProductType: string
      ModelNumber: string option
      SellingPrice: int
      PurchasePrice: int
      CostOfSales: int
      TaxType: int
      ProductCategoryCode: string
      MiscellaneousType: int
      InventoryManagementFlag: int
      InventoryAllocationFlag: int
      SupplierCode: string option
      SupplierBranch: int option
      CreatedAt: DateTime
      CreatedBy: string
      UpdatedAt: DateTime
      UpdatedBy: string }

/// ページングレスポンスDTO
type PageResponse<'T> =
    { Content: 'T list
      Page: int
      Size: int
      Total: int64
      TotalPages: int }

module ProductResponse =
    /// EntityからDTOへの変換
    let fromEntity (product: Product) : ProductResponse =
        { ProductCode = product.ProductCode
          ProductFormalName = product.ProductFormalName
          ProductAbbreviation = product.ProductAbbreviation
          ProductNameKana = product.ProductNameKana
          ProductType = product.ProductType
          ModelNumber = product.ModelNumber
          SellingPrice = product.SellingPrice
          PurchasePrice = product.PurchasePrice
          CostOfSales = product.CostOfSales
          TaxType = product.TaxType
          ProductCategoryCode = product.ProductCategoryCode
          MiscellaneousType = product.MiscellaneousType
          InventoryManagementFlag = product.InventoryManagementFlag
          InventoryAllocationFlag = product.InventoryAllocationFlag
          SupplierCode = product.SupplierCode
          SupplierBranch = product.SupplierBranch
          CreatedAt = product.CreatedAt
          CreatedBy = product.CreatedBy
          UpdatedAt = product.UpdatedAt
          UpdatedBy = product.UpdatedBy }

module PageResponse =
    /// ページングレスポンスの作成
    let create (content: 'T list) (page: int) (size: int) (total: int64) : PageResponse<'T> =
        { Content = content
          Page = page
          Size = size
          Total = total
          TotalPages = int (Math.Ceiling(float total / float size)) }
