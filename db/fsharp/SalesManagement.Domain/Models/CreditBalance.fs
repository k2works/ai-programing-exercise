namespace SalesManagement.Domain.Models

open System

/// 与信残高データ
[<CLIMutable>]
type CreditBalance = {
    CompanyCode: string          // 取引先コード（PK）
    OrderBalance: decimal        // 受注残高（受注したが未出荷の金額）
    ReceivableBalance: decimal   // 債権残高（出荷したが未入金の金額）
    PayableBalance: decimal      // 債務残高（仕入したが未支払の金額）
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string option     // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string option     // 更新者名
}
