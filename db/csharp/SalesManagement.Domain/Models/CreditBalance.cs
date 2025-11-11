namespace SalesManagement.Domain.Models;

/// <summary>
/// 与信残高データ
/// </summary>
public class CreditBalance
{
    /// <summary>
    /// 取引先コード
    /// </summary>
    public string CompanyCode { get; set; } = string.Empty;

    /// <summary>
    /// 受注残高
    /// </summary>
    public decimal OrderBalance { get; set; }

    /// <summary>
    /// 債権残高
    /// </summary>
    public decimal ReceivableBalance { get; set; }

    /// <summary>
    /// 債務残高
    /// </summary>
    public decimal PayableBalance { get; set; }

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
