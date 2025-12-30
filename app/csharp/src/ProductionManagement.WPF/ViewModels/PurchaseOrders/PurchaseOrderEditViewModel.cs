using System.Collections.ObjectModel;
using System.ComponentModel.DataAnnotations;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Domain.Models.Supplier;
using ProductionManagement.WPF.Services;
using ProductionManagement.WPF.ViewModels.Items;

namespace ProductionManagement.WPF.ViewModels.PurchaseOrders;

/// <summary>
/// 発注登録/編集 ViewModel
/// </summary>
public partial class PurchaseOrderEditViewModel : ObservableValidator
{
    private readonly IPurchaseOrderUseCase _purchaseOrderUseCase;
    private readonly ISupplierUseCase _supplierUseCase;
    private readonly IItemUseCase _itemUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    private EditMode _editMode = EditMode.Create;
    private string? _originalOrderNumber;

    public PurchaseOrderEditViewModel(
        IPurchaseOrderUseCase purchaseOrderUseCase,
        ISupplierUseCase supplierUseCase,
        IItemUseCase itemUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _purchaseOrderUseCase = purchaseOrderUseCase;
        _supplierUseCase = supplierUseCase;
        _itemUseCase = itemUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        // 初期値
        OrderDate = DateTime.Today;

        if (navigationService is NavigationService nav && nav.CurrentParameter is PurchaseOrderEditParameter param)
        {
            _editMode = param.Mode;
            _originalOrderNumber = param.OrderNumber;
        }

        _ = InitializeAsync();
    }

    #region Header Properties

    [ObservableProperty]
    private string _purchaseOrderNumber = string.Empty;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "発注日は必須です")]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private DateTime _orderDate;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "取引先は必須です")]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private Supplier? _selectedSupplier;

    [ObservableProperty]
    private string? _remarks;

    #endregion

    #region Master Data

    [ObservableProperty]
    private ObservableCollection<Supplier> _suppliers = [];

    [ObservableProperty]
    private ObservableCollection<Item> _items = [];

    #endregion

    #region Details

    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private ObservableCollection<PurchaseOrderDetailEditModel> _details = [];

    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(RemoveDetailCommand))]
    private PurchaseOrderDetailEditModel? _selectedDetail;

    #endregion

    #region UI State

    [ObservableProperty]
    private bool _isLoading;

    [ObservableProperty]
    private bool _isEditMode;

    [ObservableProperty]
    private bool _canEditHeader = true;

    /// <summary>
    /// 画面タイトル
    /// </summary>
    public string Title => _editMode == EditMode.Create ? "発注登録" : "発注編集";

    /// <summary>
    /// 合計金額
    /// </summary>
    public decimal TotalAmount => Details.Sum(d => d.OrderAmount);

    #endregion

    /// <summary>
    /// 初期化
    /// </summary>
    private async Task InitializeAsync()
    {
        try
        {
            IsLoading = true;

            // マスタデータ読み込み
            await Task.WhenAll(LoadSuppliersAsync(), LoadItemsAsync());

            // 編集モードの場合、既存データを読み込み
            if (_editMode == EditMode.Edit && _originalOrderNumber != null)
            {
                IsEditMode = true;
                await LoadOrderAsync(_originalOrderNumber);
            }
            else
            {
                // 新規の場合、空の明細を1行追加
                AddDetail();
            }
        }
        finally
        {
            IsLoading = false;
        }
    }

    /// <summary>
    /// 取引先一覧読み込み
    /// </summary>
    private async Task LoadSuppliersAsync()
    {
        var suppliers = await _supplierUseCase.GetAllSuppliersAsync();
        // 仕入先・外注先のみフィルタ
        var filtered = suppliers
            .Where(s => s.SupplierType == SupplierType.Vendor ||
                       s.SupplierType == SupplierType.Subcontractor ||
                       s.SupplierType == SupplierType.VendorAndSubcontractor)
            .OrderBy(s => s.SupplierCode)
            .ToList();
        Suppliers = new ObservableCollection<Supplier>(filtered);
    }

    /// <summary>
    /// 品目一覧読み込み
    /// </summary>
    private async Task LoadItemsAsync()
    {
        var items = await _itemUseCase.GetAllItemsAsync();
        // 購入品のみフィルタ
        var filtered = items
            .Where(i => i.ItemCategory == ItemCategory.RawMaterial ||
                       i.ItemCategory == ItemCategory.Part)
            .OrderBy(i => i.ItemCode)
            .ToList();
        Items = new ObservableCollection<Item>(filtered);
    }

    /// <summary>
    /// 発注データ読み込み
    /// </summary>
    private async Task LoadOrderAsync(string orderNumber)
    {
        try
        {
            var order = await _purchaseOrderUseCase.GetOrderAsync(orderNumber);

            PurchaseOrderNumber = order.PurchaseOrderNumber;
            OrderDate = order.OrderDate.ToDateTime(TimeOnly.MinValue);
            SelectedSupplier = Suppliers.FirstOrDefault(s => s.SupplierCode == order.SupplierCode);
            Remarks = order.Remarks;

            // 確定済みの場合は編集不可
            CanEditHeader = order.Status == PurchaseOrderStatus.Creating;

            // 明細読み込み
            Details.Clear();
            foreach (var detail in order.Details)
            {
                var item = Items.FirstOrDefault(i => i.ItemCode == detail.ItemCode);
                var editModel = new PurchaseOrderDetailEditModel
                {
                    LineNumber = detail.LineNumber,
                    SelectedItem = item,
                    OrderQuantity = detail.OrderQuantity,
                    OrderUnitPrice = detail.OrderUnitPrice,
                    ExpectedReceivingDate = detail.ExpectedReceivingDate.ToDateTime(TimeOnly.MinValue),
                    DetailRemarks = detail.DetailRemarks
                };
                editModel.PropertyChanged += Detail_PropertyChanged;
                Details.Add(editModel);
            }

            OnPropertyChanged(nameof(TotalAmount));
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
            _navigationService.GoBack();
        }
    }

    /// <summary>
    /// 明細追加
    /// </summary>
    [RelayCommand]
    private void AddDetail()
    {
        var lineNumber = Details.Count > 0 ? Details.Max(d => d.LineNumber) + 1 : 1;
        var detail = new PurchaseOrderDetailEditModel
        {
            LineNumber = lineNumber,
            ExpectedReceivingDate = DateTime.Today.AddDays(7) // デフォルト：1週間後
        };
        detail.PropertyChanged += Detail_PropertyChanged;
        Details.Add(detail);
        SelectedDetail = detail;
        SaveCommand.NotifyCanExecuteChanged();
    }

    /// <summary>
    /// 明細削除
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanRemoveDetail))]
    private void RemoveDetail()
    {
        if (SelectedDetail == null) return;

        var index = Details.IndexOf(SelectedDetail);
        SelectedDetail.PropertyChanged -= Detail_PropertyChanged;
        Details.Remove(SelectedDetail);

        // 行番号振り直し
        for (int i = 0; i < Details.Count; i++)
        {
            Details[i].LineNumber = i + 1;
        }

        // 次の行を選択
        if (Details.Count > 0)
        {
            SelectedDetail = Details[Math.Min(index, Details.Count - 1)];
        }

        OnPropertyChanged(nameof(TotalAmount));
        SaveCommand.NotifyCanExecuteChanged();
    }

    private bool CanRemoveDetail() => SelectedDetail != null && Details.Count > 1;

    private void Detail_PropertyChanged(object? sender, System.ComponentModel.PropertyChangedEventArgs e)
    {
        if (e.PropertyName == nameof(PurchaseOrderDetailEditModel.OrderAmount) ||
            e.PropertyName == nameof(PurchaseOrderDetailEditModel.SelectedItem))
        {
            OnPropertyChanged(nameof(TotalAmount));
            SaveCommand.NotifyCanExecuteChanged();
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

        // 明細の検証
        var invalidDetails = Details.Where(d => d.SelectedItem == null || d.OrderQuantity <= 0).ToList();
        if (invalidDetails.Any())
        {
            await _dialogService.ShowErrorAsync("入力エラー", "明細の品目と数量を正しく入力してください");
            return;
        }

        try
        {
            IsLoading = true;

            var command = new PurchaseOrderCreateCommand
            {
                SupplierCode = SelectedSupplier!.SupplierCode,
                OrderDate = DateOnly.FromDateTime(OrderDate),
                Remarks = Remarks,
                Details = Details.Select(d => new PurchaseOrderDetailCommand
                {
                    ItemCode = d.SelectedItem!.ItemCode,
                    OrderQuantity = d.OrderQuantity,
                    ExpectedReceivingDate = DateOnly.FromDateTime(d.ExpectedReceivingDate)
                }).ToList()
            };

            await _purchaseOrderUseCase.CreateOrderAsync(command);

            await _dialogService.ShowInfoAsync("完了", "発注を保存しました");
            _navigationService.NavigateTo("PurchaseOrderList");
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
        SelectedSupplier != null &&
        Details.Count > 0 &&
        Details.All(d => d.SelectedItem != null && d.OrderQuantity > 0) &&
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

/// <summary>
/// 発注明細編集モデル
/// </summary>
public partial class PurchaseOrderDetailEditModel : ObservableObject
{
    [ObservableProperty]
    private int _lineNumber;

    [ObservableProperty]
    private Item? _selectedItem;

    [ObservableProperty]
    private decimal _orderQuantity;

    [ObservableProperty]
    private decimal _orderUnitPrice;

    [ObservableProperty]
    private DateTime _expectedReceivingDate = DateTime.Today.AddDays(7);

    [ObservableProperty]
    private string? _detailRemarks;

    partial void OnSelectedItemChanged(Item? value)
    {
        // 品目が選択された場合、単価はユーザーが入力する
        // （Item モデルには価格情報がないため）
        OnPropertyChanged(nameof(OrderAmount));
    }

    partial void OnOrderQuantityChanged(decimal value)
    {
        OnPropertyChanged(nameof(OrderAmount));
    }

    partial void OnOrderUnitPriceChanged(decimal value)
    {
        OnPropertyChanged(nameof(OrderAmount));
    }

    /// <summary>
    /// 金額
    /// </summary>
    public decimal OrderAmount => OrderQuantity * OrderUnitPrice;
}
