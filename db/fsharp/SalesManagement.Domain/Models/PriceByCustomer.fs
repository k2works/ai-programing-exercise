namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 顧客別販売単価
/// </summary>
[<CLIMutable>]
type PriceByCustomer = {
    ProductCode: string
    CustomerCode: string
    SellingPrice: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
