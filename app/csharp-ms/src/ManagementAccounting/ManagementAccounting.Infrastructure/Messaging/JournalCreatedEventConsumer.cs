using ManagementAccounting.Application.Ports.In;
using MassTransit;
using Microsoft.Extensions.Logging;

namespace ManagementAccounting.Infrastructure.Messaging;

/// <summary>
/// 仕訳作成イベントを受信したときに処理（イベントコンシューマー）
///
/// 財務会計サービスで仕訳が作成されたら、キャッシュを更新する
/// </summary>
public class JournalCreatedEventConsumer : IConsumer<JournalCreatedEvent>
{
    private readonly IAnalyzeFinancialDataUseCase _analyzeUseCase;
    private readonly ILogger<JournalCreatedEventConsumer> _logger;

    public JournalCreatedEventConsumer(
        IAnalyzeFinancialDataUseCase analyzeUseCase,
        ILogger<JournalCreatedEventConsumer> logger)
    {
        _analyzeUseCase = analyzeUseCase;
        _logger = logger;
    }

    public async Task Consume(ConsumeContext<JournalCreatedEvent> context)
    {
        var @event = context.Message;

        _logger.LogInformation(
            "Received JournalCreatedEvent: JournalId={JournalId}, FiscalYear={FiscalYear}",
            @event.JournalId,
            @event.FiscalYear);

        // 仕訳が作成されたら、該当年度のキャッシュを再計算
        await _analyzeUseCase.AnalyzeAsync(@event.FiscalYear);

        _logger.LogInformation(
            "Updated financial analysis cache for FiscalYear={FiscalYear}",
            @event.FiscalYear);
    }
}

/// <summary>
/// 仕訳作成イベント（財務会計サービスから発行）
/// </summary>
public record JournalCreatedEvent(int JournalId, int FiscalYear, DateTime JournalDate);
