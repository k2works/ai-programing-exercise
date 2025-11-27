using System.Net;
using System.Net.Http.Json;
using AccountingSystem.Infrastructure.Seed;
using AccountingSystem.Infrastructure.Web.Dtos;
using FluentAssertions;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace AccountingSystem.Tests.Integration;

/// <summary>
/// 財務分析 API の統合テスト
/// D社のSeedデータを使用して、APIエンドポイントの動作を検証
/// </summary>
public class FinancialAnalysisApiTest : ApiTestBase
{
    protected override async Task OnInitializedAsync()
    {
        // Seed データを投入
        var configuration = CreateTestConfiguration();
        var logger = new Mock<ILogger<DatabaseSeeder>>();
        var seeder = new DatabaseSeeder(configuration, logger.Object);
        await seeder.SeedDatabaseAsync();
    }

    private IConfiguration CreateTestConfiguration()
    {
        var inMemorySettings = new Dictionary<string, string?>
        {
            { "ConnectionStrings:DefaultConnection", ConnectionString },
            { "Seed:EnableOnStartup", "false" }
        };

        return new ConfigurationBuilder()
            .AddInMemoryCollection(inMemorySettings)
            .Build();
    }

    [Fact]
    public async Task GetAnalysis_Returns200_FY2021()
    {
        // When
        var response = await Client.GetAsync("/api/v1/financial-analysis/2021");

        // Debug: レスポンス内容を確認
        var responseContent = await response.Content.ReadAsStringAsync();
        Console.WriteLine($"Response Status: {response.StatusCode}");
        Console.WriteLine($"Response Content: {responseContent}");

        // Then
        response.StatusCode.Should().Be(HttpStatusCode.OK, because: responseContent);

        var result = await response.Content.ReadFromJsonAsync<FinancialAnalysisResponse>();

        result.Should().NotBeNull();
        result!.FiscalYear.Should().Be(2021);

        // 収益性指標の検証
        result.Profitability.Should().NotBeNull();
        result.Profitability.OperatingProfitMargin.Should().BeGreaterThan(0);

        // 効率性指標の検証
        result.Efficiency.Should().NotBeNull();
        result.Efficiency.TotalAssetTurnover.Should().BeGreaterThan(0);

        // 安全性指標の検証
        result.Safety.Should().NotBeNull();
        result.Safety.CurrentRatio.Should().BeGreaterThan(0);
        result.Safety.EquityRatio.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task GetAnalysis_Returns200_FY2022()
    {
        // When
        var response = await Client.GetAsync("/api/v1/financial-analysis/2022");

        // Then
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var result = await response.Content.ReadFromJsonAsync<FinancialAnalysisResponse>();

        result.Should().NotBeNull();
        result!.FiscalYear.Should().Be(2022);
    }

    [Fact]
    public async Task GetAnalysis_NotFound_Returns404()
    {
        // When: 存在しない決算期を指定
        var response = await Client.GetAsync("/api/v1/financial-analysis/1999");

        // Then
        response.StatusCode.Should().Be(HttpStatusCode.NotFound);
    }

    [Fact]
    public async Task GetAnalysisRange_Returns200()
    {
        // When
        var response = await Client.GetAsync(
            "/api/v1/financial-analysis?fromFiscalYear=2021&toFiscalYear=2022");

        // Then
        response.StatusCode.Should().Be(HttpStatusCode.OK);

        var results = await response.Content.ReadFromJsonAsync<List<FinancialAnalysisResponse>>();

        results.Should().NotBeNull();
        results!.Count.Should().Be(2);
        results.Should().Contain(r => r.FiscalYear == 2021);
        results.Should().Contain(r => r.FiscalYear == 2022);
    }

    [Fact]
    public async Task GetAnalysisRange_InvalidRange_Returns400()
    {
        // When: 開始年度が終了年度より大きい
        var response = await Client.GetAsync(
            "/api/v1/financial-analysis?fromFiscalYear=2022&toFiscalYear=2021");

        // Then
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact]
    public async Task GetAnalysis_VerifiesCorrectCalculations_FY2021()
    {
        // When
        var response = await Client.GetAsync("/api/v1/financial-analysis/2021");
        var result = await response.Content.ReadFromJsonAsync<FinancialAnalysisResponse>();

        // Then: D社のFY2021データに基づく期待値と比較
        result.Should().NotBeNull();

        // 収益性指標（D社の事例データより）
        // 売上高総利益率: 62.29%
        result!.Profitability.GrossProfitMargin.Should().BeGreaterThan(60m);
        result.Profitability.GrossProfitMargin.Should().BeLessThan(65m);

        // 売上高営業利益率: 16.99%
        result.Profitability.OperatingProfitMargin.Should().BeGreaterThan(15m);
        result.Profitability.OperatingProfitMargin.Should().BeLessThan(20m);

        // 安全性指標
        // 流動比率: 314.33%
        result.Safety.CurrentRatio.Should().BeGreaterThan(300m);
        result.Safety.CurrentRatio.Should().BeLessThan(350m);

        // 自己資本比率: 69.48%
        result.Safety.EquityRatio.Should().BeGreaterThan(65m);
        result.Safety.EquityRatio.Should().BeLessThan(75m);
    }

    [Fact]
    public async Task GetAnalysisRange_ComparesFiscalYears()
    {
        // When
        var response = await Client.GetAsync(
            "/api/v1/financial-analysis?fromFiscalYear=2021&toFiscalYear=2022");
        var results = await response.Content.ReadFromJsonAsync<List<FinancialAnalysisResponse>>();

        // Then: 期間比較の検証
        results.Should().NotBeNull();

        var fy2021 = results!.First(r => r.FiscalYear == 2021);
        var fy2022 = results.First(r => r.FiscalYear == 2022);

        // FY2022の営業利益率はFY2021より低下している（D社の事例より）
        fy2022.Profitability.OperatingProfitMargin.Should()
            .BeLessThan(fy2021.Profitability.OperatingProfitMargin);

        // FY2022の流動比率はFY2021より改善している（D社の事例より）
        fy2022.Safety.CurrentRatio.Should()
            .BeGreaterThan(fy2021.Safety.CurrentRatio);

        // FY2022の自己資本比率はFY2021より改善している（D社の事例より）
        fy2022.Safety.EquityRatio.Should()
            .BeGreaterThan(fy2021.Safety.EquityRatio);
    }
}
