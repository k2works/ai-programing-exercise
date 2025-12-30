using System.Data.Common;
using System.IO;
using CommunityToolkit.Mvvm.ComponentModel;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels;

/// <summary>
/// ViewModel の基底クラス
/// </summary>
public abstract class ViewModelBase : ObservableObject
{
    protected readonly IDialogService DialogService;

    protected ViewModelBase(IDialogService dialogService)
    {
        DialogService = dialogService;
    }

    private bool _isLoading;

    /// <summary>
    /// ローディング中フラグ
    /// </summary>
    public bool IsLoading
    {
        get => _isLoading;
        protected set => SetProperty(ref _isLoading, value);
    }

    /// <summary>
    /// 非同期処理を例外ハンドリング付きで実行
    /// </summary>
    protected async Task ExecuteAsync(Func<Task> operation, string? errorMessage = null)
    {
        try
        {
            IsLoading = true;
            await operation();
        }
        catch (Exception ex)
        {
            var message = errorMessage ?? GetUserFriendlyMessage(ex);
            await DialogService.ShowErrorDetailAsync("エラー", message, ex.Message);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 戻り値付き非同期処理を例外ハンドリング付きで実行
    /// </summary>
    protected async Task<T?> ExecuteAsync<T>(Func<Task<T>> operation, string? errorMessage = null)
    {
        try
        {
            IsLoading = true;
            return await operation();
        }
        catch (Exception ex)
        {
            var message = errorMessage ?? GetUserFriendlyMessage(ex);
            await DialogService.ShowErrorDetailAsync("エラー", message, ex.Message);
            return default;
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// ユーザーフレンドリーなメッセージを取得
    /// </summary>
    protected static string GetUserFriendlyMessage(Exception ex)
    {
        return ex switch
        {
            DbException => "データベース接続に問題が発生しました。",
            IOException => "ファイルの読み書きに問題が発生しました。",
            UnauthorizedAccessException => "アクセス権限がありません。",
            TimeoutException => "処理がタイムアウトしました。",
            _ => "予期しないエラーが発生しました。"
        };
    }
}
