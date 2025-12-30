using System.ComponentModel.DataAnnotations;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Items;

namespace ProductionManagement.WPF.ViewModels.Suppliers;

/// <summary>
/// 取引先登録/編集 ViewModel
/// </summary>
public partial class SupplierEditViewModel : ObservableValidator
{
    private readonly ISupplierUseCase _supplierUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    private EditMode _editMode = EditMode.Create;
    private string? _originalSupplierCode;

    public SupplierEditViewModel(
        ISupplierUseCase supplierUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _supplierUseCase = supplierUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        if (navigationService is NavigationService nav && nav.CurrentParameter is SupplierEditParameter param)
        {
            _editMode = param.Mode;
            _originalSupplierCode = param.SupplierCode;
            _ = InitializeAsync();
        }
    }

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "取引先コードは必須です")]
    [MaxLength(20, ErrorMessage = "取引先コードは20文字以内で入力してください")]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private string _supplierCode = string.Empty;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "取引先名は必須です")]
    [MaxLength(100, ErrorMessage = "取引先名は100文字以内で入力してください")]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private string _supplierName = string.Empty;

    [ObservableProperty]
    [MaxLength(100, ErrorMessage = "取引先名カナは100文字以内で入力してください")]
    private string? _supplierNameKana;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "取引先区分は必須です")]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private SupplierType _supplierType = SupplierType.Vendor;

    [ObservableProperty]
    [MaxLength(10, ErrorMessage = "郵便番号は10文字以内で入力してください")]
    private string? _postalCode;

    [ObservableProperty]
    [MaxLength(200, ErrorMessage = "住所は200文字以内で入力してください")]
    private string? _address;

    [ObservableProperty]
    [MaxLength(20, ErrorMessage = "電話番号は20文字以内で入力してください")]
    private string? _phoneNumber;

    [ObservableProperty]
    [MaxLength(20, ErrorMessage = "FAX番号は20文字以内で入力してください")]
    private string? _faxNumber;

    [ObservableProperty]
    [MaxLength(50, ErrorMessage = "担当者名は50文字以内で入力してください")]
    private string? _contactPerson;

    [ObservableProperty]
    private bool _isLoading;

    [ObservableProperty]
    private bool _isEditMode;

    /// <summary>
    /// 取引先区分一覧
    /// </summary>
    public IReadOnlyList<SupplierType> SupplierTypes { get; } =
        [SupplierType.Vendor, SupplierType.Subcontractor, SupplierType.Customer, SupplierType.VendorAndSubcontractor];

    /// <summary>
    /// 画面タイトル
    /// </summary>
    public string Title => _editMode == EditMode.Create ? "取引先登録" : "取引先編集";

    /// <summary>
    /// 初期化
    /// </summary>
    private async Task InitializeAsync()
    {
        if (_editMode == EditMode.Edit && _originalSupplierCode != null)
        {
            IsEditMode = true;
            await LoadSupplierAsync(_originalSupplierCode);
        }
    }

    /// <summary>
    /// 取引先データ読み込み
    /// </summary>
    private async Task LoadSupplierAsync(string supplierCode)
    {
        try
        {
            IsLoading = true;

            var supplier = await _supplierUseCase.GetSupplierByCodeAsync(supplierCode);
            SupplierCode = supplier.SupplierCode;
            SupplierName = supplier.SupplierName;
            SupplierNameKana = supplier.SupplierNameKana;
            SupplierType = supplier.SupplierType;
            PostalCode = supplier.PostalCode;
            Address = supplier.Address;
            PhoneNumber = supplier.PhoneNumber;
            FaxNumber = supplier.FaxNumber;
            ContactPerson = supplier.ContactPerson;
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
    /// 保存
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanSave))]
    private async Task SaveAsync()
    {
        ValidateAllProperties();
        if (HasErrors)
        {
            return;
        }

        try
        {
            IsLoading = true;

            var command = new CreateSupplierCommand(
                SupplierCode: SupplierCode,
                SupplierName: SupplierName,
                SupplierType: SupplierType,
                SupplierNameKana: SupplierNameKana,
                PostalCode: PostalCode,
                Address: Address,
                PhoneNumber: PhoneNumber,
                FaxNumber: FaxNumber,
                ContactPerson: ContactPerson
            );

            await _supplierUseCase.CreateSupplierAsync(command);

            await _dialogService.ShowInfoAsync("完了", "取引先を保存しました");
            _navigationService.NavigateTo("SupplierList");
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
        finally
        {
            IsLoading = false;
        }
    }

    private bool CanSave() =>
        !string.IsNullOrWhiteSpace(SupplierCode) &&
        !string.IsNullOrWhiteSpace(SupplierName) &&
        !HasErrors;

    /// <summary>
    /// キャンセル
    /// </summary>
    [RelayCommand]
    private void Cancel()
    {
        _navigationService.GoBack();
    }
}
