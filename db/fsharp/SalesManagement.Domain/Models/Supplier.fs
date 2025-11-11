namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 仕入先マスタ（取引先の仕入先としての役割）
/// </summary>
[<CLIMutable>]
type Supplier = {
    SupplierCode: string
    SupplierBranch: int
    SupplierType: int
    SupplierName: string
    SupplierNameKana: string option
    EmployeeCode: string
    SupplierCloseDate: int
    SupplierPayMonths: int
    SupplierPayDates: int option
    SupplierPayMethod: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
