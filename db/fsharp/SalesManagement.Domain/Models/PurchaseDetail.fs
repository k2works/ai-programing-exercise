namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 仕入データ明細
/// </summary>
[<CLIMutable>]
type PurchaseDetail = {
    PurchaseNo: string             // 仕入番号（PK、FK）
    PurchaseLineNo: int            // 仕入行番号（PK）
    ProductCode: string            // 商品コード（FK）
    ProductName: string            // 商品名（履歴保持）
    LotNo: string                  // ロット番号
    WarehouseCode: string          // 倉庫コード（FK）
    PurchaseQuantity: int          // 仕入数量
    UnitPrice: int                 // 仕入単価（履歴保持）
    Amount: int                    // 金額
    ConsumptionTax: int            // 消費税
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
