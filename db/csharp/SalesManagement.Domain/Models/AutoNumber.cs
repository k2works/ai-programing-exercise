namespace SalesManagement.Domain.Models;

/// <summary>
/// 自動採番マスタ
/// </summary>
public class AutoNumber
{
    /// <summary>
    /// 伝票種別コード (OR:受注 SA:売上 PO:発注 PU:仕入 IN:請求 CR:入金 PA:支払)
    /// </summary>
    public string SlipType { get; set; } = string.Empty;

    /// <summary>
    /// 年月 (YYYYMM形式)
    /// </summary>
    public string YearMonth { get; set; } = string.Empty;

    /// <summary>
    /// 最終伝票番号
    /// </summary>
    public int LastSlipNo { get; set; }

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
