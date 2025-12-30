namespace ProductionManagement.WPF.Services;

/// <summary>
/// ダイアログサービス インターフェース
/// </summary>
public interface IDialogService
{
    Task<bool> ShowConfirmAsync(string title, string message);

    Task ShowErrorAsync(string title, string message);

    Task ShowInfoAsync(string title, string message);

    Task<string?> ShowInputAsync(string title, string prompt);
}
