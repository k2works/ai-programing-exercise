namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 売上データ明細
/// </summary>
[<CLIMutable>]
type SalesDetail = {
    SalesSlipNo: string          // 売上伝票番号（PK、FK）
    SalesLineNo: int             // 売上行番号（PK）
    ProductCode: string          // 商品コード（FK）
    ProductName: string          // 商品名（履歴保持）
    Quantity: int                // 売上数量
    UnitPrice: int               // 売単価（履歴保持）
    Amount: int                  // 金額
    ConsumptionTax: int          // 消費税
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string            // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string            // 更新者名
}
