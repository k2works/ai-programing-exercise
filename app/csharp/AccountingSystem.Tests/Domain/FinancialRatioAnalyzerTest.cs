using AccountingSystem.Domain.Models.Financial;
using FluentAssertions;
using Xunit;

namespace AccountingSystem.Tests.Domain;

/// <summary>
/// 財務分析指標計算のテスト
/// D社（化粧品製造販売会社）の事例データを使用
/// </summary>
public class FinancialRatioAnalyzerTest
{
    private readonly FinancialRatioAnalyzer _analyzer;
    private readonly FinancialData _fy2021Data;
    private readonly FinancialData _fy2022Data;

    public FinancialRatioAnalyzerTest()
    {
        _analyzer = new FinancialRatioAnalyzer();

        // 令和3年度（FY2021）のD社財務データ
        // 貸借対照表と損益計算書から抽出
        _fy2021Data = new FinancialData(
            FiscalYear: 2021,
            Sales: 5_796_105m,                  // 売上高（千円）
            CostOfSales: 2_185_856m,            // 売上原価
            SellingExpenses: 2_625_222m,        // 販売費及び一般管理費
            OperatingProfit: 985_027m,          // 営業利益
            OrdinaryProfit: 982_719m,           // 経常利益
            NetProfit: 651_660m,                // 当期純利益
            TotalAssets: 2_863_166m,            // 総資産
            TangibleFixedAssets: 64_524m,       // 有形固定資産（建物及び構築物）
            CurrentAssets: 2_676_193m,          // 流動資産
            CurrentLiabilities: 851_394m,       // 流動負債
            FixedLiabilities: 22_500m,          // 固定負債
            QuickAssets: 1_679_096m,            // 当座資産（流動資産-棚卸資産）
            Equity: 1_989_272m,                 // 純資産
            Inventory: 948_537m,                // 棚卸資産
            ReceivablesTurnover: 5.34m          // 売上債権回転率
        );

        // 令和4年度（FY2022）のD社財務データ
        _fy2022Data = new FinancialData(
            FiscalYear: 2022,
            Sales: 4_547_908m,                  // 売上高（千円）
            CostOfSales: 1_743_821m,            // 売上原価
            SellingExpenses: 2_277_050m,        // 販売費及び一般管理費
            OperatingProfit: 527_037m,          // 営業利益
            OrdinaryProfit: 537_032m,           // 経常利益
            NetProfit: 367_960m,                // 当期純利益
            TotalAssets: 2_974_899m,            // 総資産
            TangibleFixedAssets: 63_256m,       // 有形固定資産（建物及び構築物）
            CurrentAssets: 2_777_545m,          // 流動資産
            CurrentLiabilities: 640_513m,       // 流動負債
            FixedLiabilities: 27_153m,          // 固定負債
            QuickAssets: 1_998_185m,            // 当座資産（流動資産-棚卸資産）
            Equity: 2_307_233m,                 // 純資産
            Inventory: 740_810m,                // 棚卸資産
            ReceivablesTurnover: 5.26m          // 売上債権回転率
        );
    }

    #region 収益性指標のテスト

    [Fact]
    public void 売上高総利益率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 62.29% ((5796105 - 2185856) / 5796105 * 100)
        result.Profitability.GrossProfitMargin
            .Should().BeApproximately(62.29m, 0.1m);
    }

    [Fact]
    public void 売上高営業利益率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 16.99% (985027 / 5796105 * 100)
        result.Profitability.OperatingProfitMargin
            .Should().BeApproximately(16.99m, 0.1m);
    }

    [Fact]
    public void 売上高経常利益率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 16.95% (982719 / 5796105 * 100)
        result.Profitability.OrdinaryProfitMargin
            .Should().BeApproximately(16.95m, 0.1m);
    }

    [Fact]
    public void 売上高販管費比率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 45.29% (2625222 / 5796105 * 100)
        result.Profitability.SellingExpenseRatio
            .Should().BeApproximately(45.29m, 0.1m);
    }

    [Fact]
    public void 売上高営業利益率が正しく計算される_FY2022()
    {
        // When
        var result = _analyzer.Analyze(_fy2022Data);

        // Then: 11.59% (527037 / 4547908 * 100)
        result.Profitability.OperatingProfitMargin
            .Should().BeApproximately(11.59m, 0.1m);
    }

    #endregion

    #region 効率性指標のテスト

    [Fact]
    public void 総資本回転率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 2.02回 (5796105 / 2863166)
        result.Efficiency.TotalAssetTurnover
            .Should().BeApproximately(2.02m, 0.01m);
    }

    [Fact]
    public void 売上債権回転率が正しく設定される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 5.34回
        result.Efficiency.ReceivablesTurnover
            .Should().BeApproximately(5.34m, 0.01m);
    }

    [Fact]
    public void 棚卸資産回転率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 6.11回 (5796105 / 948537)
        result.Efficiency.InventoryTurnover
            .Should().BeApproximately(6.11m, 0.1m);
    }

    [Fact]
    public void 有形固定資産回転率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 89.83回 (5796105 / 64524)
        result.Efficiency.TangibleFixedAssetTurnover
            .Should().BeApproximately(89.83m, 0.5m);
    }

    [Fact]
    public void 総資本回転率が正しく計算される_FY2022()
    {
        // When
        var result = _analyzer.Analyze(_fy2022Data);

        // Then: 1.53回 (4547908 / 2974899)
        result.Efficiency.TotalAssetTurnover
            .Should().BeApproximately(1.53m, 0.01m);
    }

    #endregion

    #region 安全性指標のテスト

    [Fact]
    public void 流動比率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 314.33% (2676193 / 851394 * 100)
        result.Safety.CurrentRatio
            .Should().BeApproximately(314.33m, 0.5m);
    }

    [Fact]
    public void 当座比率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 197.22% (1679096 / 851394 * 100)
        result.Safety.QuickRatio
            .Should().BeApproximately(197.22m, 0.5m);
    }

    [Fact]
    public void 固定比率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 9.40% (186973 / 1989272 * 100)
        // 固定資産 = 総資産 - 流動資産 = 2863166 - 2676193 = 186973
        result.Safety.FixedRatio
            .Should().BeApproximately(9.40m, 0.1m);
    }

    [Fact]
    public void 固定長期適合率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 9.29% (186973 / (1989272 + 22500) * 100)
        result.Safety.FixedLongTermRatio
            .Should().BeApproximately(9.29m, 0.1m);
    }

    [Fact]
    public void 負債比率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 43.93% ((851394 + 22500) / 1989272 * 100)
        result.Safety.DebtRatio
            .Should().BeApproximately(43.93m, 0.5m);
    }

    [Fact]
    public void 自己資本比率が正しく計算される_FY2021()
    {
        // When
        var result = _analyzer.Analyze(_fy2021Data);

        // Then: 69.48% (1989272 / 2863166 * 100)
        result.Safety.EquityRatio
            .Should().BeApproximately(69.48m, 0.1m);
    }

    [Fact]
    public void 流動比率が正しく計算される_FY2022()
    {
        // When
        var result = _analyzer.Analyze(_fy2022Data);

        // Then: 433.64% (2777545 / 640513 * 100)
        result.Safety.CurrentRatio
            .Should().BeApproximately(433.64m, 0.5m);
    }

    [Fact]
    public void 自己資本比率が正しく計算される_FY2022()
    {
        // When
        var result = _analyzer.Analyze(_fy2022Data);

        // Then: 77.56% (2307233 / 2974899 * 100)
        result.Safety.EquityRatio
            .Should().BeApproximately(77.56m, 0.1m);
    }

    #endregion

    #region 期間比較のテスト

    [Fact]
    public void 収益性指標の期間比較_営業利益率が悪化している()
    {
        // Given
        var fy2021Result = _analyzer.Analyze(_fy2021Data);
        var fy2022Result = _analyzer.Analyze(_fy2022Data);

        // When
        var change = fy2022Result.Profitability.OperatingProfitMargin
                   - fy2021Result.Profitability.OperatingProfitMargin;

        // Then: -5.40ポイント (11.59 - 16.99)
        change.Should().BeApproximately(-5.40m, 0.2m);
    }

    [Fact]
    public void 安全性指標の期間比較_流動比率が改善している()
    {
        // Given
        var fy2021Result = _analyzer.Analyze(_fy2021Data);
        var fy2022Result = _analyzer.Analyze(_fy2022Data);

        // When
        var change = fy2022Result.Safety.CurrentRatio
                   - fy2021Result.Safety.CurrentRatio;

        // Then: +119.31ポイント (433.64 - 314.33)
        change.Should().BeApproximately(119.31m, 1.0m);
    }

    [Fact]
    public void 効率性指標の期間比較_総資本回転率が悪化している()
    {
        // Given
        var fy2021Result = _analyzer.Analyze(_fy2021Data);
        var fy2022Result = _analyzer.Analyze(_fy2022Data);

        // When
        var change = fy2022Result.Efficiency.TotalAssetTurnover
                   - fy2021Result.Efficiency.TotalAssetTurnover;

        // Then: -0.49回 (1.53 - 2.02)
        change.Should().BeApproximately(-0.49m, 0.02m);
    }

    #endregion

    #region バリデーションのテスト

    [Fact]
    public void 売上高がゼロの場合は例外がスローされる()
    {
        // Given
        var invalidData = _fy2021Data with { Sales = 0m };

        // When & Then
        var act = () => _analyzer.Analyze(invalidData);
        act.Should().Throw<ArgumentException>()
            .WithMessage("*売上高がゼロのため計算できません*");
    }

    [Fact]
    public void 総資産がゼロの場合は例外がスローされる()
    {
        // Given
        var invalidData = _fy2021Data with { TotalAssets = 0m };

        // When & Then
        var act = () => _analyzer.Analyze(invalidData);
        act.Should().Throw<ArgumentException>()
            .WithMessage("*総資産がゼロのため計算できません*");
    }

    [Fact]
    public void 流動負債がゼロの場合は例外がスローされる()
    {
        // Given
        var invalidData = _fy2021Data with { CurrentLiabilities = 0m };

        // When & Then
        var act = () => _analyzer.Analyze(invalidData);
        act.Should().Throw<ArgumentException>()
            .WithMessage("*流動負債がゼロのため計算できません*");
    }

    #endregion

    #region FinancialDataのテスト

    [Fact]
    public void FinancialData_売上総利益が正しく計算される()
    {
        // Given & When
        var grossProfit = _fy2021Data.GrossProfit;

        // Then: 3,610,249 = 5,796,105 - 2,185,856
        grossProfit.Should().Be(3_610_249m);
    }

    [Fact]
    public void FinancialData_総負債が正しく計算される()
    {
        // Given & When
        var totalLiabilities = _fy2021Data.TotalLiabilities;

        // Then: 873,894 = 851,394 + 22,500
        totalLiabilities.Should().Be(873_894m);
    }

    [Fact]
    public void FinancialData_固定資産が正しく計算される()
    {
        // Given & When
        var fixedAssets = _fy2021Data.FixedAssets;

        // Then: 186,973 = 2,863,166 - 2,676,193
        fixedAssets.Should().Be(186_973m);
    }

    #endregion
}
