namespace SalesManagement.Domain.Models

open System

/// 入金口座マスタ
[<CLIMutable>]
type BankAccount = {
    AccountCode: string          // 口座コード（PK）
    AccountName: string          // 口座名
    BankName: string             // 銀行名
    BranchName: string           // 支店名
    AccountNumber: string        // 口座番号
    AccountType: int             // 口座種別（1:普通 2:当座）
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string            // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string            // 更新者名
}
