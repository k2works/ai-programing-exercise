namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 仕入データ
/// </summary>
[<CLIMutable>]
type Purchase = {
    PurchaseNo: string             // 仕入番号（PK）
    PurchaseDate: DateTime         // 仕入日
    PoNo: string                   // 発注番号（FK、追跡用）
    SupplierCode: string           // 仕入先コード（FK）
    SupplierBranch: int            // 仕入先枝番（FK）
    EmployeeCode: string           // 社員コード（FK）
    PurchaseAmount: int            // 仕入金額合計
    ConsumptionTax: int            // 消費税合計
    SlipComment: string option     // 備考
    DepartmentCode: string         // 部門コード（FK）
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
