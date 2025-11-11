namespace SalesManagement.Domain.Models;

/// <summary>
/// 請求データ明細
/// </summary>
public class InvoiceDetail
{
    /// <summary>
    /// 請求番号
    /// </summary>
    public string InvoiceNo { get; set; } = string.Empty;

    /// <summary>
    /// 請求明細番号
    /// </summary>
    public int InvoiceDetailNo { get; set; }

    /// <summary>
    /// 売上番号
    /// </summary>
    public string SalesNo { get; set; } = string.Empty;

    /// <summary>
    /// 売上行番号
    /// </summary>
    public int SalesLineNo { get; set; }

    /// <summary>
    /// 請求額
    /// </summary>
    public decimal InvoiceAmount { get; set; }

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
