namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 部門マスタのEntityレコード型
/// </summary>
[<CLIMutable>]
type Department = {
    DepartmentCode: string
    StartDate: DateTime
    EndDate: DateTime
    DepartmentName: string
    OrganizationLevel: int
    DepartmentPath: string
    LowestLevelFlag: int
    SlipInputFlag: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
