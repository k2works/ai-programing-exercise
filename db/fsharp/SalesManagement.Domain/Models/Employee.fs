namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 社員マスタのEntityレコード型
/// </summary>
[<CLIMutable>]
type Employee = {
    EmployeeCode: string
    EmployeeName: string
    EmployeeNameKana: string
    Gender: string
    BirthDate: DateTime option
    JoinDate: DateTime
    DepartmentCode: string
    PositionCode: string option
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
