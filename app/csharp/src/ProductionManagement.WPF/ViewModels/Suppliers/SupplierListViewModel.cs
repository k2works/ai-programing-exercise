using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Items;

namespace ProductionManagement.WPF.ViewModels.Suppliers;

/// <summary>
/// 取引先一覧 ViewModel
/// </summary>
public partial class SupplierListViewModel : ObservableObject
{
    private readonly ISupplierUseCase _supplierUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    public SupplierListViewModel(
        ISupplierUseCase supplierUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _supplierUseCase = supplierUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        _ = SearchAsync();
    }

    [ObservableProperty]
    private ObservableCollection<Supplier> _suppliers = [];

    [ObservableProperty]
    private Supplier? _selectedSupplier;

    [ObservableProperty]
    private string _keyword = string.Empty;

    [ObservableProperty]
    private SupplierType? _selectedSupplierType;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// 取引先区分一覧（フィルタ用）
    /// </summary>
    public IReadOnlyList<SupplierType?> SupplierTypes { get; } =
        [null, SupplierType.Vendor, SupplierType.Subcontractor, SupplierType.Customer, SupplierType.VendorAndSubcontractor];

    partial void OnSelectedSupplierTypeChanged(SupplierType? value)
    {
        _ = SearchAsync();
    }

    /// <summary>
    /// データ読み込み
    /// </summary>
    [RelayCommand]
    private async Task LoadAsync()
    {
        try
        {
            IsLoading = true;

            IReadOnlyList<Supplier> suppliers;
            if (SelectedSupplierType.HasValue)
            {
                suppliers = await _supplierUseCase.GetSuppliersByTypeAsync(SelectedSupplierType.Value);
            }
            else
            {
                suppliers = await _supplierUseCase.GetAllSuppliersAsync();
            }

            Suppliers = new ObservableCollection<Supplier>(suppliers);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 検索
    /// </summary>
    [RelayCommand]
    private async Task SearchAsync()
    {
        try
        {
            IsLoading = true;

            IReadOnlyList<Supplier> suppliers;
            if (SelectedSupplierType.HasValue)
            {
                suppliers = await _supplierUseCase.GetSuppliersByTypeAsync(SelectedSupplierType.Value);
            }
            else
            {
                suppliers = await _supplierUseCase.GetAllSuppliersAsync();
            }

            // キーワードでフィルタ
            if (!string.IsNullOrWhiteSpace(Keyword))
            {
                suppliers = suppliers
                    .Where(s => s.SupplierCode.Contains(Keyword, StringComparison.OrdinalIgnoreCase) ||
                                s.SupplierName.Contains(Keyword, StringComparison.OrdinalIgnoreCase))
                    .ToList();
            }

            Suppliers = new ObservableCollection<Supplier>(suppliers);
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// フィルタクリア
    /// </summary>
    [RelayCommand]
    private async Task ClearFilterAsync()
    {
        Keyword = string.Empty;
        SelectedSupplierType = null;
        await LoadAsync();
    }

    /// <summary>
    /// 詳細表示
    /// </summary>
    [RelayCommand]
    private void ShowDetail(Supplier? supplier)
    {
        if (supplier != null)
        {
            _navigationService.NavigateTo("SupplierDetail", supplier.SupplierCode);
        }
    }

    /// <summary>
    /// 新規作成
    /// </summary>
    [RelayCommand]
    private void Create()
    {
        _navigationService.NavigateTo("SupplierEdit", new SupplierEditParameter(EditMode.Create));
    }

    /// <summary>
    /// 行ダブルクリック
    /// </summary>
    [RelayCommand]
    private void RowDoubleClick(Supplier? supplier)
    {
        ShowDetail(supplier);
    }
}

/// <summary>
/// 取引先編集パラメータ
/// </summary>
public record SupplierEditParameter(EditMode Mode, string? SupplierCode = null);
