using ManagementAccounting.Domain.Entities;

namespace ManagementAccounting.Application.Ports.Out;

/// <summary>
/// 財務会計サービスクライアントインターフェース（出力ポート）
///
/// 腐敗防止層として機能し、財務会計コンテキストからのデータを
/// 管理会計コンテキストのドメインモデルに変換
/// </summary>
public interface IFinancialAccountingClient
{
    /// <summary>
    /// 指定された会計年度の財務データを取得
    /// </summary>
    Task<FinancialData> FetchFinancialDataByFiscalYearAsync(int fiscalYear);
}
