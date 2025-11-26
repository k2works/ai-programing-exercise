using AccountingSystem.Domain.Models.Financial;

namespace AccountingSystem.Infrastructure.Web.Dtos;

/// <summary>
/// 財務指標レスポンス DTO
/// </summary>
public class FinancialRatiosResponse
{
    /// <summary>
    /// 流動比率（%）
    /// </summary>
    public decimal CurrentRatio { get; set; }

    /// <summary>
    /// 自己資本比率（%）
    /// </summary>
    public decimal EquityRatio { get; set; }

    /// <summary>
    /// 売上総利益率（%）
    /// </summary>
    public decimal GrossProfitMargin { get; set; }

    /// <summary>
    /// 営業利益率（%）
    /// </summary>
    public decimal OperatingProfitMargin { get; set; }

    /// <summary>
    /// 当期純利益率（%）
    /// </summary>
    public decimal NetProfitMargin { get; set; }

    /// <summary>
    /// 総資産利益率（ROA, %）
    /// </summary>
    public decimal Roa { get; set; }

    /// <summary>
    /// 自己資本利益率（ROE, %）
    /// </summary>
    public decimal Roe { get; set; }

    /// <summary>
    /// Domain Model からの変換
    /// </summary>
    public static FinancialRatiosResponse From(FinancialRatios ratios)
    {
        return new FinancialRatiosResponse
        {
            CurrentRatio = ratios.CurrentRatio,
            EquityRatio = ratios.EquityRatio,
            GrossProfitMargin = ratios.GrossProfitMargin,
            OperatingProfitMargin = ratios.OperatingProfitMargin,
            NetProfitMargin = ratios.NetProfitMargin,
            Roa = ratios.Roa,
            Roe = ratios.Roe
        };
    }
}
