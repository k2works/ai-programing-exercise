namespace SalesManagement.Domain.Models

open System

/// <summary>
/// 売上データ
/// </summary>
[<CLIMutable>]
type Sales = {
    SalesSlipNo: string          // 売上伝票番号（PK）
    SalesDate: DateTime          // 売上日
    OrderNo: string option       // 受注番号（FK、追跡用）
    CustomerCode: string         // 得意先コード（FK）
    CustomerBranch: int          // 得意先枝番（FK）
    EmployeeCode: string         // 社員コード（FK）
    SalesAmount: int             // 売上金額合計
    ConsumptionTax: int          // 消費税合計
    SlipComment: string option   // 伝票備考
    DepartmentCode: string       // 部門コード（FK）
    CreatedAt: DateTime          // 作成日時
    CreatedBy: string            // 作成者名
    UpdatedAt: DateTime          // 更新日時
    UpdatedBy: string            // 更新者名
}
