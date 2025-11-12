namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 発注データ
/// </summary>
[<CLIMutable>]
type PurchaseOrder = {
    PoNo: string                   // 発注番号（PK）
    PoDate: DateTime               // 発注日
    OrderNo: string                // 受注番号（FK、追跡用）
    SupplierCode: string           // 仕入先コード（FK）
    SupplierBranch: int            // 仕入先枝番（FK）
    EmployeeCode: string           // 社員コード（FK）
    DueDate: DateTime option       // 指定納期
    WarehouseCode: string          // 倉庫コード（FK）
    PoAmount: int                  // 発注金額合計
    ConsumptionTax: int            // 消費税合計
    SlipComment: string option     // 備考
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
