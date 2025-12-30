using System.ComponentModel.DataAnnotations;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Exceptions;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Items;

/// <summary>
/// 品目登録/編集 ViewModel
/// </summary>
public partial class ItemEditViewModel : ObservableValidator
{
    private readonly IItemUseCase _itemUseCase;
    private readonly INavigationService _navigationService;
    private readonly IDialogService _dialogService;

    private EditMode _mode;
    private string? _originalItemCode;

    public ItemEditViewModel(
        IItemUseCase itemUseCase,
        INavigationService navigationService,
        IDialogService dialogService)
    {
        _itemUseCase = itemUseCase;
        _navigationService = navigationService;
        _dialogService = dialogService;

        if (navigationService is NavigationService nav && nav.CurrentParameter is ItemEditParameter param)
        {
            Initialize(param);
        }
    }

    /// <summary>
    /// 初期化
    /// </summary>
    public async void Initialize(ItemEditParameter parameter)
    {
        _mode = parameter.Mode;

        if (_mode == EditMode.Edit && parameter.ItemCode != null)
        {
            _originalItemCode = parameter.ItemCode;
            await LoadItemAsync(parameter.ItemCode);
            IsItemCodeReadOnly = true;
        }
        else
        {
            IsItemCodeReadOnly = false;
        }

        Title = _mode == EditMode.Create ? "品目登録" : "品目編集";
    }

    /// <summary>
    /// 画面タイトル
    /// </summary>
    [ObservableProperty]
    private string _title = "品目登録";

    /// <summary>
    /// 品目コードが読み取り専用か
    /// </summary>
    [ObservableProperty]
    private bool _isItemCodeReadOnly;

    /// <summary>
    /// 処理中フラグ
    /// </summary>
    [ObservableProperty]
    [NotifyCanExecuteChangedFor(nameof(SaveCommand))]
    private bool _isSaving;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "品目コードは必須です")]
    [MaxLength(20, ErrorMessage = "品目コードは20文字以内で入力してください")]
    private string _itemCode = string.Empty;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "品名は必須です")]
    [MaxLength(100, ErrorMessage = "品名は100文字以内で入力してください")]
    private string _itemName = string.Empty;

    [ObservableProperty]
    [NotifyDataErrorInfo]
    [Required(ErrorMessage = "品目区分は必須です")]
    private ItemCategory? _category;

    [ObservableProperty]
    private string? _unitCode;

    [ObservableProperty]
    [Range(0, int.MaxValue, ErrorMessage = "リードタイムは0以上で入力してください")]
    private int _leadTime;

    [ObservableProperty]
    [Range(0, int.MaxValue, ErrorMessage = "安全リードタイムは0以上で入力してください")]
    private int _safetyLeadTime;

    [ObservableProperty]
    [Range(0, double.MaxValue, ErrorMessage = "安全在庫は0以上で入力してください")]
    private decimal _safetyStock;

    /// <summary>
    /// 品目区分リスト
    /// </summary>
    public IReadOnlyList<ItemCategory> Categories { get; } = Enum.GetValues<ItemCategory>();

    /// <summary>
    /// 品目読み込み（編集時）
    /// </summary>
    private async Task LoadItemAsync(string itemCode)
    {
        try
        {
            var item = await _itemUseCase.GetItemByCodeAsync(itemCode);

            ItemCode = item.ItemCode;
            ItemName = item.ItemName;
            Category = item.ItemCategory;
            UnitCode = item.UnitCode;
            LeadTime = item.LeadTime;
            SafetyLeadTime = item.SafetyLeadTime;
            SafetyStock = item.SafetyStock;

            ClearErrors();
        }
        catch (ItemNotFoundException)
        {
            await _dialogService.ShowErrorAsync("エラー", "品目が見つかりません");
            _navigationService.GoBack();
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
            await _dialogService.ShowErrorAsync("入力エラー", "入力内容を確認してください");
            return;
        }

        try
        {
            IsSaving = true;

            if (_mode == EditMode.Create)
            {
                var command = new CreateItemCommand(
                    ItemCode: ItemCode,
                    ItemName: ItemName,
                    Category: Category!.Value,
                    UnitCode: UnitCode,
                    LeadTime: LeadTime,
                    SafetyLeadTime: SafetyLeadTime,
                    SafetyStock: SafetyStock
                );

                await _itemUseCase.CreateItemAsync(command);
                await _dialogService.ShowInfoAsync("完了", $"品目「{ItemCode}」を登録しました");
            }
            else
            {
                var command = new UpdateItemCommand(
                    ItemCode: _originalItemCode!,
                    ItemName: ItemName,
                    Category: Category!.Value,
                    UnitCode: UnitCode,
                    LeadTime: LeadTime,
                    SafetyLeadTime: SafetyLeadTime,
                    SafetyStock: SafetyStock
                );

                await _itemUseCase.UpdateItemAsync(command);
                await _dialogService.ShowInfoAsync("完了", $"品目「{_originalItemCode}」を更新しました");
            }

            _navigationService.GoBack();
        }
        catch (DuplicateItemException)
        {
            await _dialogService.ShowErrorAsync("エラー", $"品目コード「{ItemCode}」は既に存在します");
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
        }
        finally
        {
            IsSaving = false;
        }
    }

    private bool CanSave() => !IsSaving;

    /// <summary>
    /// キャンセル
    /// </summary>
    [RelayCommand]
    private async Task CancelAsync()
    {
        if (!string.IsNullOrEmpty(ItemCode) || !string.IsNullOrEmpty(ItemName))
        {
            var confirmed = await _dialogService.ShowConfirmAsync(
                "確認",
                "入力内容を破棄しますか？");

            if (!confirmed)
            {
                return;
            }
        }

        _navigationService.GoBack();
    }
}
