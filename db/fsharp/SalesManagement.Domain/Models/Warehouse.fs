namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 倉庫マスタ
/// </summary>
[<CLIMutable>]
type Warehouse = {
    WarehouseCode: string          // 倉庫コード（PK）
    WarehouseName: string          // 倉庫名
    WarehouseType: int             // 倉庫区分（1=自社倉庫、2=委託倉庫など）
    Address: string option         // 住所
    PhoneNumber: string option     // 電話番号
    ManagerCode: string option     // 責任者コード（FK）
    CreatedAt: DateTime            // 作成日時
    CreatedBy: string              // 作成者名
    UpdatedAt: DateTime            // 更新日時
    UpdatedBy: string              // 更新者名
}
