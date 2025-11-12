namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 在庫データ（5フィールド複合主キー）
/// </summary>
[<CLIMutable>]
type Stock = {
    WarehouseCode: string          // 倉庫コード（PK）
    ProductCode: string            // 商品コード（PK）
    LotNo: string                  // ロット番号（PK）
    StockType: string              // 在庫区分（PK、1=通常在庫、2=預託在庫など）
    QualityType: string            // 良品区分（PK、G=良品、B=不良品、H=保留品）
    ActualQuantity: int            // 実在庫数
    ValidQuantity: int             // 有効在庫数（実在庫 - 引当数量）
    LastDeliveryDate: DateTime option  // 最終出荷日
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
