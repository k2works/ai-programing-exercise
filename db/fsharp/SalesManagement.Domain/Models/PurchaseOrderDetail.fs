namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 発注データ明細
/// </summary>
[<CLIMutable>]
type PurchaseOrderDetail = {
    PoNo: string                   // 発注番号（PK、FK）
    PoLineNo: int                  // 発注行番号（PK）
    ProductCode: string            // 商品コード（FK）
    ProductName: string            // 商品名（履歴保持）
    PoQuantity: int                // 発注数量
    ReceivedQuantity: int          // 入荷済数量
    UnitPrice: int                 // 仕入単価（履歴保持）
    Amount: int                    // 金額
    ConsumptionTax: int            // 消費税
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
