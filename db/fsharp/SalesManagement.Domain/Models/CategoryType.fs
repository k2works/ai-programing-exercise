namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 分類種別マスタ（業種、規模などの分類軸を定義）
/// </summary>
[<CLIMutable>]
type CategoryType = {
    CategoryTypeCode: string
    CategoryTypeName: string
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
