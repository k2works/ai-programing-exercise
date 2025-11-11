namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 商品分類マスタ
/// </summary>
[<CLIMutable>]
type ProductCategory = {
    ProductCategoryCode: string
    ProductCategoryName: string
    ProductCategoryLevel: int
    ProductCategoryPath: string
    LowestLevelFlag: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
