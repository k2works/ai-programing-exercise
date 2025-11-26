using System.ComponentModel.DataAnnotations;

namespace AccountingSystem.Api.Dtos;

/// <summary>
/// 勘定科目作成/更新リクエスト DTO
/// </summary>
public record AccountRequest
{
    /// <summary>
    /// 勘定科目コード
    /// </summary>
    [Required(ErrorMessage = "勘定科目コードは必須です")]
    [StringLength(10, ErrorMessage = "勘定科目コードは10文字以内で指定してください")]
    public required string AccountCode { get; init; }

    /// <summary>
    /// 勘定科目名
    /// </summary>
    [Required(ErrorMessage = "勘定科目名は必須です")]
    [StringLength(100, ErrorMessage = "勘定科目名は100文字以内で指定してください")]
    public required string AccountName { get; init; }

    /// <summary>
    /// 勘定科目カナ
    /// </summary>
    [StringLength(100, ErrorMessage = "勘定科目カナは100文字以内で指定してください")]
    public string? AccountNameKana { get; init; }

    /// <summary>
    /// 勘定科目種別
    /// </summary>
    [Required(ErrorMessage = "勘定科目種別は必須です")]
    [StringLength(20, ErrorMessage = "勘定科目種別は20文字以内で指定してください")]
    public required string AccountType { get; init; }

    /// <summary>
    /// 合計科目フラグ
    /// </summary>
    public bool IsSummaryAccount { get; init; }

    /// <summary>
    /// BSPL区分（B: 貸借対照表、P: 損益計算書）
    /// </summary>
    [StringLength(1, ErrorMessage = "BSPL区分は1文字で指定してください")]
    public string? BsplType { get; init; }

    /// <summary>
    /// 取引要素区分
    /// </summary>
    [StringLength(10, ErrorMessage = "取引要素区分は10文字以内で指定してください")]
    public string? TransactionElementType { get; init; }

    /// <summary>
    /// 費用区分
    /// </summary>
    [StringLength(10, ErrorMessage = "費用区分は10文字以内で指定してください")]
    public string? ExpenseType { get; init; }

    /// <summary>
    /// 表示順序
    /// </summary>
    public int DisplayOrder { get; init; }

    /// <summary>
    /// 集計対象フラグ
    /// </summary>
    public bool IsAggregationTarget { get; init; } = true;

    /// <summary>
    /// 課税取引コード
    /// </summary>
    [StringLength(10, ErrorMessage = "課税取引コードは10文字以内で指定してください")]
    public string? TaxCode { get; init; }
}
