namespace SalesManagement.Domain.Models

open System

/// 請求データ明細
[<CLIMutable>]
type InvoiceDetail = {
    InvoiceNo: string            // 請求番号（PK、FK）
    InvoiceDetailNo: int         // 請求明細番号（PK）
    SalesSlipNo: string          // 売上伝票番号（FK）
    SalesDetailNo: int           // 売上明細番号（FK）
    InvoiceAmount: decimal       // 請求額
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string            // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string            // 更新者名
}
