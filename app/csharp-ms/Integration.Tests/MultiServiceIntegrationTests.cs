using System.Net.Http.Json;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Testcontainers.PostgreSql;
using Testcontainers.RabbitMq;

namespace Integration.Tests;

/// <summary>
/// 財務会計サービス統合テスト（TestContainers使用）
/// </summary>
public class FinancialAccountingIntegrationTests : IAsyncLifetime
{
    private PostgreSqlContainer _financialPostgres = null!;
    private RabbitMqContainer _rabbitMq = null!;

    private WebApplicationFactory<FinancialAccounting.Api.Controllers.JournalController> _financialFactory = null!;

    private HttpClient _financialClient = null!;

    public async Task InitializeAsync()
    {
        // PostgreSQL コンテナ（財務会計用）
        _financialPostgres = new PostgreSqlBuilder()
            .WithImage("postgres:16-alpine")
            .WithDatabase("financial_accounting")
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
            _rabbitMq.StartAsync()
        );

        // 財務会計サービス用の WebApplicationFactory
        var connectionString = _financialPostgres.GetConnectionString();
        var rabbitHost = _rabbitMq.Hostname;
        var rabbitPort = _rabbitMq.GetMappedPublicPort(5672).ToString();

        _financialFactory = new WebApplicationFactory<FinancialAccounting.Api.Controllers.JournalController>()
            .WithWebHostBuilder(builder =>
            {
                builder.UseSetting("ConnectionStrings:FinancialAccounting", connectionString);
                builder.UseSetting("RabbitMQ:Host", rabbitHost);
                builder.UseSetting("RabbitMQ:Port", rabbitPort);
                builder.UseSetting("RabbitMQ:Username", "guest");
                builder.UseSetting("RabbitMQ:Password", "guest");
            });

        _financialClient = _financialFactory.CreateClient();
    }

    public async Task DisposeAsync()
    {
        _financialClient?.Dispose();

        await _financialFactory.DisposeAsync();

        await Task.WhenAll(
            _financialPostgres.DisposeAsync().AsTask(),
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
        var content = await response.Content.ReadAsStringAsync();
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.Created, content);

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
        var createResponse = await _financialClient.PostAsJsonAsync("/api/journals", createRequest);
        createResponse.EnsureSuccessStatusCode();

        // Act
        var response = await _financialClient.GetAsync("/api/journals?fiscalYear=2024");

        // Assert
        var content = await response.Content.ReadAsStringAsync();
        response.StatusCode.Should().Be(System.Net.HttpStatusCode.OK, content);

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

        // Assert - ドメインバリデーションにより例外がスローされ、500エラーが返される
        // 将来的には適切なエラーハンドリングミドルウェアで400を返すべき
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
