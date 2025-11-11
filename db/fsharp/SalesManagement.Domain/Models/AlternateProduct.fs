namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 代替商品
/// </summary>
[<CLIMutable>]
type AlternateProduct = {
    ProductCode: string
    AlternateProductCode: string
    Priority: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
