namespace AccountingSystem.Infrastructure.Entities;

/// <summary>
/// 課税取引エンティティクラス
/// データベースの日本語カラム名と英語プロパティ名をマッピング
/// </summary>
public record TaxTransaction
{
    /// <summary>
    /// 課税取引コード（01:課税、02:非課税、03:免税、04:不課税）
    /// </summary>
    public string TaxCode { get; init; } = string.Empty;

    /// <summary>
    /// 課税取引名
    /// </summary>
    public string TaxName { get; init; } = string.Empty;

    /// <summary>
    /// 税率（0.10 = 10%）
    /// </summary>
    public decimal TaxRate { get; init; }

    /// <summary>
    /// 説明
    /// </summary>
    public string? Description { get; init; }

    /// <summary>
    /// 有効フラグ
    /// </summary>
    public bool IsActive { get; init; } = true;

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime UpdatedAt { get; init; }
}
