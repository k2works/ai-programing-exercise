using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Items;

namespace ProductionManagement.WPF.ViewModels.Suppliers;

/// <summary>
/// 取引先詳細 ViewModel
/// </summary>
public partial class SupplierDetailViewModel : ObservableObject
{
    private readonly ISupplierUseCase _supplierUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    public SupplierDetailViewModel(
        ISupplierUseCase supplierUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _supplierUseCase = supplierUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        if (navigationService is NavigationService nav && nav.CurrentParameter is string supplierCode)
        {
            _ = LoadAsync(supplierCode);
        }
    }

    [ObservableProperty]
    private Supplier? _supplier;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// データ読み込み
    /// </summary>
    public async Task LoadAsync(string supplierCode)
    {
        try
        {
            IsLoading = true;
            Supplier = await _supplierUseCase.GetSupplierByCodeAsync(supplierCode);
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
            _navigationService.GoBack();
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 編集画面へ遷移
    /// </summary>
    [RelayCommand]
    private void Edit()
    {
        if (Supplier != null)
        {
            _navigationService.NavigateTo("SupplierEdit", new SupplierEditParameter(
                Mode: EditMode.Edit,
                SupplierCode: Supplier.SupplierCode
            ));
        }
    }

    /// <summary>
    /// 戻る
    /// </summary>
    [RelayCommand]
    private void GoBack()
    {
        _navigationService.GoBack();
    }
}
