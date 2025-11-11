namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 顧客マスタ（取引先の顧客としての役割）
/// </summary>
[<CLIMutable>]
type Customer = {
    CustomerCode: string
    CustomerBranch: int
    CustomerType: int
    ArCode: string
    ArBranch: int option
    PayerCode: string
    PayerBranch: int option
    CustomerName: string
    CustomerNameKana: string option
    EmployeeCode: string
    CustomerUserName: string option
    CustomerDepartmentName: string option
    CustomerZipCode: string option
    CustomerState: string option
    CustomerAddress1: string option
    CustomerAddress2: string option
    CustomerTel: string option
    CustomerFax: string option
    CustomerEmail: string option
    CustomerArType: int
    CustomerCloseDate1: int
    CustomerPayMonths1: int
    CustomerPayDates1: int option
    CustomerPayMethod1: int
    CustomerCloseDate2: int
    CustomerPayMonths2: int
    CustomerPayDates2: int option
    CustomerPayMethod2: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
