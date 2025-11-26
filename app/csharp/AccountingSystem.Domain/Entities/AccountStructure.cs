namespace AccountingSystem.Domain.Entities;

/// <summary>
/// 勘定科目構成エンティティクラス
/// データベースの日本語カラム名と英語プロパティ名をマッピング
/// </summary>
public record AccountStructure
{
    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public string AccountCode { get; init; } = string.Empty;

    /// <summary>
    /// 勘定科目パス（チルダ連結形式: 11~11000~11190~11110）
    /// </summary>
    public string AccountPath { get; init; } = string.Empty;

    /// <summary>
    /// 階層レベル（ルート=1）
    /// </summary>
    public int HierarchyLevel { get; init; }

    /// <summary>
    /// 親科目コード
    /// </summary>
    public string? ParentAccountCode { get; init; }

    /// <summary>
    /// 表示順序（同じ階層内での表示順序）
    /// </summary>
    public int DisplayOrder { get; init; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime CreatedAt { get; init; }

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime UpdatedAt { get; init; }
}
