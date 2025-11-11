namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 取引先分類所属マスタ（取引先と分類の多対多関係）
/// </summary>
[<CLIMutable>]
type CompanyCategoryGroup = {
    CategoryTypeCode: string
    CategoryCode: string
    CompanyCode: string
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
