namespace SalesManagement.Domain.Models

open System

/// 請求データ
[<CLIMutable>]
type Invoice = {
    InvoiceNo: string            // 請求番号（PK）
    InvoiceDate: DateTime        // 請求日
    CustomerCode: string         // 得意先コード（FK）
    CustomerBranch: int          // 得意先枝番（FK）
    SalesSlipNo: string option   // 売上伝票番号（FK、追跡用）
    InvoiceAmount: decimal       // 請求額
    ClearedAmount: decimal       // 請求消込金額
    Remarks: string option       // 備考
    DepartmentCode: string       // 部門コード（FK）
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string            // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string            // 更新者名
}
