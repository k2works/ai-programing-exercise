namespace SalesManagement.Domain.Models

open System

/// 自動採番マスタ
[<CLIMutable>]
type AutoNumber = {
    SlipType: string             // 伝票種別コード（PK）(OR:受注 SA:売上 PO:発注 PU:仕入 IN:請求 CR:入金 PA:支払)
    YearMonth: string            // 年月（PK、YYYYMM形式）
    LastSlipNo: int              // 最終伝票番号
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string option     // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string option     // 更新者名
}
