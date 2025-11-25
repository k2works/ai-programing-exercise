using System.Diagnostics.CodeAnalysis;

namespace AccountingSystem.Infrastructure.Entities;

/// <summary>
/// 勘定科目エンティティクラス
/// データベースの日本語カラム名と英語プロパティ名をマッピング
/// </summary>
public record Account
{
    /// <summary>
    /// 勘定科目ID
    /// </summary>
    public int? AccountId { get; init; }

    /// <summary>
    /// 勘定科目コード
    /// </summary>
    public required string AccountCode { get; init; }

    /// <summary>
    /// 勘定科目名
    /// </summary>
    public required string AccountName { get; init; }

    /// <summary>
    /// 勘定科目カナ
    /// </summary>
    public string? AccountNameKana { get; init; }

    /// <summary>
    /// 勘定科目種別
    /// </summary>
    public required string AccountType { get; init; }

    /// <summary>
    /// 合計科目
    /// </summary>
    public required bool IsSummaryAccount { get; init; }

    /// <summary>
    /// BSPL区分
    /// </summary>
    public string? BsplType { get; init; }

    /// <summary>
    /// 取引要素区分
    /// </summary>
    public string? TransactionElementType { get; init; }

    /// <summary>
    /// 費用区分
    /// </summary>
    public string? ExpenseType { get; init; }

    /// <summary>
    /// 表示順序
    /// </summary>
    public int DisplayOrder { get; init; }

    /// <summary>
    /// 集計対象
    /// </summary>
    public bool IsAggregationTarget { get; init; } = true;

    /// <summary>
    /// 課税取引コード
    /// </summary>
    public string? TaxCode { get; init; }

    /// <summary>
    /// 残高
    /// </summary>
    public decimal Balance { get; init; }

    /// <summary>
    /// 作成日時
    /// </summary>
    public DateTime? CreatedAt { get; init; }

    /// <summary>
    /// 更新日時
    /// </summary>
    public DateTime? UpdatedAt { get; init; }

    /// <summary>
    /// パラメータなしコンストラクタ
    /// </summary>
    public Account() { }

    /// <summary>
    /// 基本情報を指定するコンストラクタ
    /// </summary>
    [SetsRequiredMembers]
    public Account(string accountCode, string accountName, string accountType, bool isSummaryAccount)
    {
        AccountCode = accountCode;
        AccountName = accountName;
        AccountType = accountType;
        IsSummaryAccount = isSummaryAccount;
        IsAggregationTarget = true;
        Balance = 0m;
    }
}
