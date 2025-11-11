namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 受注データ明細
/// </summary>
[<CLIMutable>]
type OrderDetail = {
    OrderNo: string                // 受注番号（PK、FK）
    OrderLineNo: int               // 受注行番号（PK）
    ProductCode: string            // 商品コード（FK）
    ProductName: string            // 商品名（履歴保持）
    Quantity: int                  // 受注数量
    AllocatedQuantity: int         // 引当数量
    ShippingInstructionQuantity: int  // 出荷指示数量
    ShippedQuantity: int           // 出荷済数量
    UnitPrice: int                 // 売単価（履歴保持）
    Amount: int                    // 金額
    ConsumptionTax: int            // 消費税
    IsCompleted: int               // 完了フラグ (0: 未完了, 1: 完了)
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
