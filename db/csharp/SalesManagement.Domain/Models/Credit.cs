namespace SalesManagement.Domain.Models;

/// <summary>
/// 入金データ
/// </summary>
public class Credit
{
    /// <summary>
    /// 入金伝票番号
    /// </summary>
    public string CreditSlipNo { get; set; } = string.Empty;

    /// <summary>
    /// 入金日
    /// </summary>
    public DateTime CreditDate { get; set; }

    /// <summary>
    /// 得意先コード
    /// </summary>
    public string CustomerCode { get; set; } = string.Empty;

    /// <summary>
    /// 入金額
    /// </summary>
    public decimal CreditAmount { get; set; }

    /// <summary>
    /// 入金消込金額
    /// </summary>
    public decimal ClearedAmount { get; set; }

    /// <summary>
    /// 口座コード
    /// </summary>
    public string? AccountCode { get; set; }

    /// <summary>
    /// 入金方法 (1:現金 2:振込 3:手形 4:相殺)
    /// </summary>
    public int CreditMethod { get; set; }

    /// <summary>
    /// 備考
    /// </summary>
    public string? Remarks { get; set; }

    /// <summary>
    /// 部門コード
    /// </summary>
    public string DepartmentCode { get; set; } = string.Empty;

    /// <summary>
    /// 社員コード
    /// </summary>
    public string EmployeeCode { get; set; } = string.Empty;

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
