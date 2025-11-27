using AccountingSystem.Domain.Entities;

namespace AccountingSystem.Application.Ports.Out;

/// <summary>
/// 月次勘定科目残高リポジトリインターフェース（出力ポート）
/// </summary>
public interface IMonthlyAccountBalanceRepository
{
    /// <summary>
    /// 指定した決算期・月度の月次残高を取得
    /// </summary>
    /// <param name="fiscalYear">決算期</param>
    /// <param name="month">月度（1-12）</param>
    /// <returns>月次勘定科目残高のリスト</returns>
    Task<IReadOnlyList<MonthlyAccountBalance>> FindByFiscalYearAndMonthAsync(int fiscalYear, int month);

    /// <summary>
    /// 指定した決算期の全月次残高を取得
    /// </summary>
    /// <param name="fiscalYear">決算期</param>
    /// <returns>月次勘定科目残高のリスト</returns>
    Task<IReadOnlyList<MonthlyAccountBalance>> FindByFiscalYearAsync(int fiscalYear);

    /// <summary>
    /// 指定した勘定科目コードの月次残高を取得
    /// </summary>
    /// <param name="fiscalYear">決算期</param>
    /// <param name="accountCode">勘定科目コード</param>
    /// <returns>月次勘定科目残高のリスト</returns>
    Task<IReadOnlyList<MonthlyAccountBalance>> FindByAccountCodeAsync(int fiscalYear, string accountCode);
}
