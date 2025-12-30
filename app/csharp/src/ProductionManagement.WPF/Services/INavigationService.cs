namespace ProductionManagement.WPF.Services;

/// <summary>
/// 画面遷移サービス インターフェース
/// </summary>
public interface INavigationService
{
    event EventHandler<string>? NavigationRequested;

    event EventHandler<(string ViewName, object Parameter)>? NavigationWithParameterRequested;

    object? CurrentParameter { get; }

    void NavigateTo(string viewName);

    void NavigateTo(string viewName, object parameter);

    void GoBack();
}
