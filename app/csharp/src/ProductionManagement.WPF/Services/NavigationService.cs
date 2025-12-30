namespace ProductionManagement.WPF.Services;

/// <summary>
/// 画面遷移サービス実装
/// </summary>
public class NavigationService : INavigationService
{
    private readonly Stack<string> _navigationStack = new();
    private object? _currentParameter;

    public event EventHandler<string>? NavigationRequested;

    public event EventHandler<(string ViewName, object Parameter)>? NavigationWithParameterRequested;

    public object? CurrentParameter => _currentParameter;

    public void NavigateTo(string viewName)
    {
        _navigationStack.Push(viewName);
        _currentParameter = null;
        NavigationRequested?.Invoke(this, viewName);
    }

    public void NavigateTo(string viewName, object parameter)
    {
        _navigationStack.Push(viewName);
        _currentParameter = parameter;
        NavigationWithParameterRequested?.Invoke(this, (viewName, parameter));
    }

    public void GoBack()
    {
        if (_navigationStack.Count > 1)
        {
            _navigationStack.Pop();
            var previousView = _navigationStack.Peek();
            _currentParameter = null;
            NavigationRequested?.Invoke(this, previousView);
        }
    }
}
