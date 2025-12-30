using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels;

/// <summary>
/// メインウィンドウ ViewModel
/// </summary>
public partial class MainViewModel : ObservableObject
{
    private readonly INavigationService _navigationService;

    [ObservableProperty]
    private object? _currentView;

    [ObservableProperty]
    private string _statusMessage = "準備完了";

    [ObservableProperty]
    private bool _isLoading;

    [ObservableProperty]
    private string _currentUser = "admin";

    [ObservableProperty]
    private DateTime _currentDate = DateTime.Today;

    public MainViewModel(INavigationService navigationService)
    {
        _navigationService = navigationService;
        _navigationService.NavigationRequested += OnNavigationRequested;
    }

    public void SetLoading(bool isLoading, string? message = null)
    {
        IsLoading = isLoading;
        if (message != null)
        {
            StatusMessage = message;
        }
    }

    [RelayCommand]
    private void Navigate(string viewName)
    {
        _navigationService.NavigateTo(viewName);
    }

    private void OnNavigationRequested(object? sender, string viewName)
    {
        // 現時点では ViewModel の作成は後続の章で実装
        // ここではステータスメッセージのみ更新
        StatusMessage = $"{viewName} を表示中";
    }
}
