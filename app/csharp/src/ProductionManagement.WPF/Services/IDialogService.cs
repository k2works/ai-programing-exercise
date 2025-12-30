namespace ProductionManagement.WPF.Services;

/// <summary>
/// ダイアログサービス インターフェース
/// </summary>
public interface IDialogService
{
    Task<bool> ShowConfirmAsync(string title, string message);

    Task ShowErrorAsync(string title, string message);

    /// <summary>
    /// 詳細付きエラーダイアログを表示
    /// </summary>
    Task ShowErrorDetailAsync(string title, string message, string detail);

    Task ShowInfoAsync(string title, string message);

    /// <summary>
    /// 警告ダイアログを表示
    /// </summary>
    Task ShowWarningAsync(string title, string message);

    Task<string?> ShowInputAsync(string title, string prompt);
}
