using AccountingSystem.Domain.Entities;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 勘定科目レスポンス DTO
/// </summary>
public record AccountResponse
{
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
    /// 合計科目フラグ
    /// </summary>
    public bool IsSummaryAccount { get; init; }

    /// <summary>
    /// BSPL区分（B: 貸借対照表、P: 損益計算書）
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
    /// 集計対象フラグ
    /// </summary>
    public bool IsAggregationTarget { get; init; }

    /// <summary>
    /// 課税取引コード
    /// </summary>
    public string? TaxCode { get; init; }

    /// <summary>
    /// 残高
    /// </summary>
    public decimal Balance { get; init; }

    /// <summary>
    /// Entity から DTO を生成
    /// </summary>
    public static AccountResponse From(Account account) => new()
    {
        AccountCode = account.AccountCode,
        AccountName = account.AccountName,
        AccountNameKana = account.AccountNameKana,
        AccountType = account.AccountType,
        IsSummaryAccount = account.IsSummaryAccount,
        BsplType = account.BsplType,
        TransactionElementType = account.TransactionElementType,
        ExpenseType = account.ExpenseType,
        DisplayOrder = account.DisplayOrder,
        IsAggregationTarget = account.IsAggregationTarget,
        TaxCode = account.TaxCode,
        Balance = account.Balance
    };

    /// <summary>
    /// DTO から Entity を生成
    /// </summary>
    public static Account ToEntity(AccountRequest request) => new()
    {
        AccountCode = request.AccountCode,
        AccountName = request.AccountName,
        AccountNameKana = request.AccountNameKana,
        AccountType = request.AccountType,
        IsSummaryAccount = request.IsSummaryAccount,
        BsplType = request.BsplType,
        TransactionElementType = request.TransactionElementType,
        ExpenseType = request.ExpenseType,
        DisplayOrder = request.DisplayOrder,
        IsAggregationTarget = request.IsAggregationTarget,
        TaxCode = request.TaxCode
    };
}
