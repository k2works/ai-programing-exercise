namespace SalesManagement.Domain.Models;

/// <summary>
/// 支払データ
/// </summary>
public class Payment
{
    /// <summary>
    /// 支払伝票番号
    /// </summary>
    public string PaymentSlipNo { get; set; } = string.Empty;

    /// <summary>
    /// 支払日
    /// </summary>
    public DateTime PaymentDate { get; set; }

    /// <summary>
    /// 仕入先コード
    /// </summary>
    public string SupplierCode { get; set; } = string.Empty;

    /// <summary>
    /// 支払額
    /// </summary>
    public decimal PaymentAmount { get; set; }

    /// <summary>
    /// 支払方法 (1:現金 2:振込 3:手形 4:相殺)
    /// </summary>
    public int PaymentMethod { get; set; }

    /// <summary>
    /// 支払完了フラグ (0:未完了 1:完了)
    /// </summary>
    public int CompletedFlag { get; set; }

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
