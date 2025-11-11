namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 受注データ
/// </summary>
[<CLIMutable>]
type Order = {
    OrderNo: string              // 受注番号（PK）
    OrderDate: DateTime          // 受注日
    CustomerCode: string         // 得意先コード（FK）
    CustomerBranch: int          // 得意先枝番（FK）
    EmployeeCode: string         // 社員コード（FK）
    DueDate: DateTime option     // 納期
    OrderAmount: int             // 受注金額合計
    ConsumptionTax: int          // 消費税合計
    SlipComment: string option   // 伝票備考
    DepartmentCode: string       // 部門コード（FK）
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string            // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string            // 更新者名
}
