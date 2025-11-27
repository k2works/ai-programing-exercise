using AccountingSystem.Application.Ports.Out;
using Microsoft.Extensions.Logging;

namespace AccountingSystem.Infrastructure.EventBus;

/// <summary>
/// コンソール出力用メールサービス実装（開発・テスト用）
/// 実際にメールは送信せず、ログに出力
/// </summary>
public class ConsoleEmailService : IEmailService
{
    private readonly ILogger<ConsoleEmailService> _logger;

    public ConsoleEmailService(ILogger<ConsoleEmailService> logger)
    {
        _logger = logger;
    }

    public Task SendAsync(string recipient, string subject, string body)
    {
        _logger.LogInformation(@"
========================================
メール送信（開発用ダミー）
----------------------------------------
宛先: {Recipient}
件名: {Subject}
本文:
{Body}
========================================",
            recipient, subject, body);

        return Task.CompletedTask;
    }
}
