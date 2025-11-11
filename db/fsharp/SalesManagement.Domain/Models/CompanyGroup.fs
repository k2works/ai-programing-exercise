namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 取引先グループマスタ
/// </summary>
[<CLIMutable>]
type CompanyGroup = {
    CompanyGroupCode: string
    CompanyGroupName: string
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
