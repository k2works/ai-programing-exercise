namespace SalesManagement.Domain.Models;

/// <summary>
/// 請求データ
/// </summary>
public class Invoice
{
    /// <summary>
    /// 請求番号
    /// </summary>
    public string InvoiceNo { get; set; } = string.Empty;

    /// <summary>
    /// 請求日
    /// </summary>
    public DateTime InvoiceDate { get; set; }

    /// <summary>
    /// 得意先コード
    /// </summary>
    public string CustomerCode { get; set; } = string.Empty;

    /// <summary>
    /// 売上番号
    /// </summary>
    public string? SalesNo { get; set; }

    /// <summary>
    /// 請求額
    /// </summary>
    public decimal InvoiceAmount { get; set; }

    /// <summary>
    /// 請求消込金額
    /// </summary>
    public decimal ClearedAmount { get; set; }

    /// <summary>
    /// 備考
    /// </summary>
    public string? Remarks { get; set; }

    /// <summary>
    /// 部門コード
    /// </summary>
    public string DepartmentCode { get; set; } = string.Empty;

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
