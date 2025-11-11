namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 取引先マスタ（Partyモデルの基盤）
/// </summary>
[<CLIMutable>]
type Company = {
    CompanyCode: string
    CompanyName: string
    CompanyNameKana: string option
    SupplierType: int
    ZipCode: string option
    State: string option
    Address1: string option
    Address2: string option
    NoSalesFlag: int
    WideUseType: int
    CompanyGroupCode: string
    MaxCredit: int
    TempCreditUp: int
    CreatedAt: DateTime
    CreatedBy: string
    UpdatedAt: DateTime
    UpdatedBy: string
}
