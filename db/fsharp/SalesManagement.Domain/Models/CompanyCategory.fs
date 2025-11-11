namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 取引先分類マスタ（分類種別ごとの分類を定義）
/// </summary>
[<CLIMutable>]
type CompanyCategory = {
    CategoryTypeCode: string
    CategoryCode: string
    CategoryName: string
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
