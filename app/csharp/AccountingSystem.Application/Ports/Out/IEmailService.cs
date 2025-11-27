namespace AccountingSystem.Application.Ports.Out;

/// <summary>
/// メール送信サービスインターフェース
/// </summary>
public interface IEmailService
{
    /// <summary>
    /// メールを送信
    /// </summary>
    /// <param name="recipient">宛先</param>
    /// <param name="subject">件名</param>
    /// <param name="body">本文</param>
    Task SendAsync(string recipient, string subject, string body);
}
