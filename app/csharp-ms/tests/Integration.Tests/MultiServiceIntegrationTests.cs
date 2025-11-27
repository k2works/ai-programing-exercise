using System.Net.Http.Json;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.Extensions.Configuration;
using Testcontainers.PostgreSql;
using Testcontainers.RabbitMq;

namespace Integration.Tests;

/// <summary>
/// マルチサービス統合テスト（TestContainers使用）
///
/// 財務会計サービスと管理会計サービスの連携をテスト
/// </summary>
public class MultiServiceIntegrationTests : IAsyncLifetime
{
    private PostgreSqlContainer _financialPostgres = null!;
    private PostgreSqlContainer _managementPostgres = null!;
    private RabbitMqContainer _rabbitMq = null!;

    private WebApplicationFactory<FinancialAccounting.Api.Controllers.JournalController> _financialFactory = null!;
    private WebApplicationFactory<ManagementAccounting.Api.Controllers.FinancialAnalysisController> _managementFactory = null!;

    private HttpClient _financialClient = null!;
    private HttpClient _managementClient = null!;

    public async Task InitializeAsync()
    {
        // PostgreSQL コンテナ（財務会計用）
        _financialPostgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("financial_accounting")
            .WithUsername("test")
            .WithPassword("test")
            .Build();

        // PostgreSQL コンテナ（管理会計用）
        _managementPostgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("management_accounting")
            .WithUsername("test")
            .WithPassword("test")
            .Build();

        // RabbitMQ コンテナ
        _rabbitMq = new RabbitMqBuilder()
            .WithImage("rabbitmq:3-management-alpine")
            .WithUsername("guest")
            .WithPassword("guest")
            .Build();

        // コンテナを並列で起動
        await Task.WhenAll(
            _financialPostgres.StartAsync(),
            _managementPostgres.StartAsync(),
            _rabbitMq.StartAsync()
        );

        // 財務会計サービス用の WebApplicationFactory
        _financialFactory = new WebApplicationFactory<FinancialAccounting.Api.Controllers.JournalController>()
            .WithWebHostBuilder(builder =>
            {
                builder.ConfigureAppConfiguration((context, config) =>
                {
                    config.AddInMemoryCollection(new Dictionary<string, string?>
                    {
                        ["ConnectionStrings:FinancialAccounting"] = _financialPostgres.GetConnectionString(),
                        ["RabbitMQ:Host"] = _rabbitMq.Hostname,
                        ["RabbitMQ:Port"] = _rabbitMq.GetMappedPublicPort(5672).ToString(),
                        ["RabbitMQ:Username"] = "guest",
                        ["RabbitMQ:Password"] = "guest"
                    });
                });
            });

        _financialClient = _financialFactory.CreateClient();

        // 管理会計サービス用の WebApplicationFactory
        _managementFactory = new WebApplicationFactory<ManagementAccounting.Api.Controllers.FinancialAnalysisController>()
            .WithWebHostBuilder(builder =>
            {
                builder.ConfigureAppConfiguration((context, config) =>
                {
                    config.AddInMemoryCollection(new Dictionary<string, string?>
                    {
                        ["ConnectionStrings:ManagementAccounting"] = _managementPostgres.GetConnectionString(),
                        ["FinancialAccountingService:BaseUrl"] = _financialClient.BaseAddress!.ToString(),
                        ["RabbitMQ:Host"] = _rabbitMq.Hostname,
                        ["RabbitMQ:Port"] = _rabbitMq.GetMappedPublicPort(5672).ToString(),
                        ["RabbitMQ:Username"] = "guest",
                        ["RabbitMQ:Password"] = "guest"
                    });
                });
            });

        _managementClient = _managementFactory.CreateClient();
    }

    public async Task DisposeAsync()
    {
        _financialClient?.Dispose();
        _managementClient?.Dispose();

        await _financialFactory.DisposeAsync();
        await _managementFactory.DisposeAsync();

        await Task.WhenAll(
            _financialPostgres.DisposeAsync().AsTask(),
            _managementPostgres.DisposeAsync().AsTask(),
            _rabbitMq.DisposeAsync().AsTask()
        );
    }

    [Fact(DisplayName = "財務会計サービスで仕訳を作成できる")]
    public async Task CanCreateJournalInFinancialAccounting()
    {
        // Arrange
        var request = new
        {
            JournalDate = DateTime.UtcNow.Date,
            Description = "売上計上",
            FiscalYear = 2024,
            Entries = new[]
            {
                new { AccountCode = "1120", DebitAmount = 100000m, CreditAmount = 0m, Description = "売掛金増加" },
                new { AccountCode = "4110", DebitAmount = 0m, CreditAmount = 100000m, Description = "売上高" }
            }
        };

        // Act
        var response = await _financialClient.PostAsJsonAsync("/api/journals", request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created);

        var journal = await response.Content.ReadFromJsonAsync<JournalResponse>();
        journal.Should().NotBeNull();
        journal!.JournalId.Should().BeGreaterThan(0);
        journal.Entries.Should().HaveCount(2);
    }

    [Fact(DisplayName = "財務会計サービスで仕訳を取得できる")]
    public async Task CanGetJournalByFiscalYear()
    {
        // Arrange - まず仕訳を作成
        var createRequest = new
        {
            JournalDate = DateTime.UtcNow.Date,
            Description = "テスト仕訳",
            FiscalYear = 2024,
            Entries = new[]
            {
                new { AccountCode = "1110", DebitAmount = 50000m, CreditAmount = 0m, Description = "現金増加" },
                new { AccountCode = "4110", DebitAmount = 0m, CreditAmount = 50000m, Description = "売上" }
            }
        };
        await _financialClient.PostAsJsonAsync("/api/journals", createRequest);

        // Act
        var response = await _financialClient.GetAsync("/api/journals?fiscalYear=2024");

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK);

        var journals = await response.Content.ReadFromJsonAsync<List<JournalResponse>>();
        journals.Should().NotBeNull();
        journals!.Should().NotBeEmpty();
    }

    [Fact(DisplayName = "借方と貸方が一致しない仕訳は作成できない")]
    public async Task CannotCreateUnbalancedJournal()
    {
        // Arrange - 借方と貸方が一致しない仕訳
        var request = new
        {
            JournalDate = DateTime.UtcNow.Date,
            Description = "不正な仕訳",
            FiscalYear = 2024,
            Entries = new[]
            {
                new { AccountCode = "1110", DebitAmount = 100000m, CreditAmount = 0m, Description = "現金" },
                new { AccountCode = "4110", DebitAmount = 0m, CreditAmount = 50000m, Description = "売上（金額不一致）" }
            }
        };

        // Act
        var response = await _financialClient.PostAsJsonAsync("/api/journals", request);

        // Assert
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.InternalServerError);
    }
}

/// <summary>
/// 仕訳レスポンス DTO
/// </summary>
public record JournalResponse(
    int JournalId,
    DateTime JournalDate,
    string Description,
    int FiscalYear,
    List<JournalEntryResponse> Entries
);

/// <summary>
/// 仕訳明細レスポンス DTO
/// </summary>
public record JournalEntryResponse(
    int EntryId,
    string AccountCode,
    decimal DebitAmount,
    decimal CreditAmount,
    string? Description
);
