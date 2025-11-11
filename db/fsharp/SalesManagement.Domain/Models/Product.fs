namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 商品マスタ
/// </summary>
[<CLIMutable>]
type Product = {
    ProductCode: string
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
    UpdatedBy: string
}
