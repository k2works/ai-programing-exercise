namespace SalesManagement.Domain.Models;

/// <summary>
/// 入金口座マスタ
/// </summary>
public class BankAccount
{
    /// <summary>
    /// 口座コード
    /// </summary>
    public string AccountCode { get; set; } = string.Empty;

    /// <summary>
    /// 口座名
    /// </summary>
    public string AccountName { get; set; } = string.Empty;

    /// <summary>
    /// 銀行名
    /// </summary>
    public string BankName { get; set; } = string.Empty;

    /// <summary>
    /// 支店名
    /// </summary>
    public string BranchName { get; set; } = string.Empty;

    /// <summary>
    /// 口座番号
    /// </summary>
    public string AccountNumber { get; set; } = string.Empty;

    /// <summary>
    /// 口座種別 (1:普通 2:当座)
    /// </summary>
    public int AccountType { get; set; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; set; }

    /// <summary>
    /// 作成者名
    /// </summary>
    public string CreatedBy { get; set; } = string.Empty;

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// 更新者名
    /// </summary>
    public string UpdatedBy { get; set; } = string.Empty;
}
