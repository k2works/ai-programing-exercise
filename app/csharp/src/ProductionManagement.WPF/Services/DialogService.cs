using System.Windows;

namespace ProductionManagement.WPF.Services;

/// <summary>
/// ダイアログサービス実装
/// </summary>
public class DialogService : IDialogService
{
    public Task<bool> ShowConfirmAsync(string title, string message)
    {
        var result = MessageBox.Show(
            message,
            title,
            MessageBoxButton.YesNo,
            MessageBoxImage.Question);

        return Task.FromResult(result == MessageBoxResult.Yes);
    }

    public Task ShowErrorAsync(string title, string message)
    {
        MessageBox.Show(message, title, MessageBoxButton.OK, MessageBoxImage.Error);
        return Task.CompletedTask;
    }

    public Task ShowInfoAsync(string title, string message)
    {
        MessageBox.Show(message, title, MessageBoxButton.OK, MessageBoxImage.Information);
        return Task.CompletedTask;
    }

    public Task<string?> ShowInputAsync(string title, string prompt)
    {
        // カスタム入力ダイアログは後続の章で実装
        // ここでは簡易実装
        return Task.FromResult<string?>(null);
    }
}
