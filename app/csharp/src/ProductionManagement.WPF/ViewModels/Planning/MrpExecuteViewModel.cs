using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Planning;

/// <summary>
/// MRP 実行 ViewModel
/// </summary>
public partial class MrpExecuteViewModel : ObservableObject
{
    private readonly IDialogService _dialogService;
    private readonly IItemRepository _itemRepository;

    public MrpExecuteViewModel(IDialogService dialogService, IItemRepository itemRepository)
    {
        _dialogService = dialogService;
        _itemRepository = itemRepository;

        // 初期値
        PlanningStartDate = DateTime.Today;
        PlanningEndDate = DateTime.Today.AddMonths(3);

        // 品目一覧を読み込み
        _ = LoadItemsAsync();
    }

    /// <summary>
    /// 品目一覧（ドロップダウン用）
    /// </summary>
    public ObservableCollection<Item> Items { get; } = [];

    private async Task LoadItemsAsync()
    {
        try
        {
            var items = await _itemRepository.FindAllAsync();
            Items.Clear();
            foreach (var item in items.OrderBy(i => i.ItemCode))
            {
                Items.Add(item);
            }
        }
        catch
        {
            // 読み込み失敗時は空のまま
        }
    }

    [ObservableProperty]
    private DateTime _planningStartDate;

    [ObservableProperty]
    private DateTime _planningEndDate;

    /// <summary>
    /// 選択された対象品目（null = 全品目）
    /// </summary>
    [ObservableProperty]
    private Item? _selectedItem;

    [ObservableProperty]
    private bool _includeSubItems = true;

    [ObservableProperty]
    private bool _isExecuting;

    [ObservableProperty]
    private string _statusMessage = "MRP 実行の準備ができました";

    [ObservableProperty]
    private int _progressPercent;

    /// <summary>
    /// MRP 実行
    /// </summary>
    [RelayCommand(CanExecute = nameof(CanExecute))]
    private async Task ExecuteAsync()
    {
        try
        {
            IsExecuting = true;
            StatusMessage = "MRP を実行中...";
            ProgressPercent = 0;

            // シミュレーション（実際の MRP UseCase がないため）
            for (int i = 0; i <= 100; i += 10)
            {
                ProgressPercent = i;
                StatusMessage = i switch
                {
                    0 => "初期化中...",
                    10 => "需要データを読み込み中...",
                    30 => "在庫データを確認中...",
                    50 => "所要量を計算中...",
                    70 => "オーダを生成中...",
                    90 => "結果を保存中...",
                    100 => "完了",
                    _ => StatusMessage
                };
                await Task.Delay(300);
            }

            await _dialogService.ShowInfoAsync("完了", "MRP 実行が完了しました。\n\nオーダ照会画面で結果を確認してください。");
            StatusMessage = "MRP 実行が完了しました";
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", ex.Message);
            StatusMessage = "MRP 実行に失敗しました";
        }
        finally
        {
            IsExecuting = false;
        }
    }

    private bool CanExecute() => !IsExecuting;

    /// <summary>
    /// リセット
    /// </summary>
    [RelayCommand]
    private void Reset()
    {
        PlanningStartDate = DateTime.Today;
        PlanningEndDate = DateTime.Today.AddMonths(3);
        SelectedItem = null;
        IncludeSubItems = true;
        ProgressPercent = 0;
        StatusMessage = "MRP 実行の準備ができました";
    }
}
