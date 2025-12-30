using System.Collections.ObjectModel;
using CommunityToolkit.Mvvm.ComponentModel;
using CommunityToolkit.Mvvm.Input;
using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Inventory;
using ProductionManagement.Domain.Models.Item;
using ProductionManagement.Domain.Models.Location;
using ProductionManagement.WPF.Services;

namespace ProductionManagement.WPF.ViewModels.Inventory;

/// <summary>
/// 在庫照会 ViewModel
/// </summary>
public partial class StockListViewModel : ObservableObject
{
    private readonly IStockRepository _stockRepository;
    private readonly ILocationRepository _locationRepository;
    private readonly IItemRepository _itemRepository;
    private readonly IDialogService _dialogService;

    public StockListViewModel(
        IStockRepository stockRepository,
        ILocationRepository locationRepository,
        IItemRepository itemRepository,
        IDialogService dialogService)
    {
        _stockRepository = stockRepository;
        _locationRepository = locationRepository;
        _itemRepository = itemRepository;
        _dialogService = dialogService;

        // ドロップダウンデータを読み込み
        _ = LoadDropdownDataAsync();
    }

    /// <summary>
    /// 在庫一覧
    /// </summary>
    public ObservableCollection<Stock> Stocks { get; } = [];

    /// <summary>
    /// 拠点一覧（ドロップダウン用）
    /// </summary>
    public ObservableCollection<Location> Locations { get; } = [];

    /// <summary>
    /// 品目一覧（ドロップダウン用）
    /// </summary>
    public ObservableCollection<Item> Items { get; } = [];

    /// <summary>
    /// 選択された拠点（null = すべて）
    /// </summary>
    [ObservableProperty]
    private Location? _selectedLocation;

    /// <summary>
    /// 選択された品目（null = すべて）
    /// </summary>
    [ObservableProperty]
    private Item? _selectedItem;

    [ObservableProperty]
    private bool _showZeroStock;

    [ObservableProperty]
    private Stock? _selectedStock;

    [ObservableProperty]
    private bool _isLoading;

    /// <summary>
    /// 合計在庫数
    /// </summary>
    [ObservableProperty]
    private decimal _totalStockQuantity;

    /// <summary>
    /// 合計良品数
    /// </summary>
    [ObservableProperty]
    private decimal _totalPassedQuantity;

    /// <summary>
    /// 合計不良品数
    /// </summary>
    [ObservableProperty]
    private decimal _totalDefectiveQuantity;

    /// <summary>
    /// 合計未検査数
    /// </summary>
    [ObservableProperty]
    private decimal _totalUninspectedQuantity;

    private async Task LoadDropdownDataAsync()
    {
        try
        {
            // 拠点一覧を読み込み
            var locations = await _locationRepository.FindAllAsync();
            Locations.Clear();
            foreach (var location in locations.OrderBy(l => l.LocationCode))
            {
                Locations.Add(location);
            }

            // 品目一覧を読み込み
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

    /// <summary>
    /// データ読み込み
    /// </summary>
    [RelayCommand]
    private async Task LoadAsync()
    {
        try
        {
            IsLoading = true;
            var stocks = await _stockRepository.FindAllAsync();

            // フィルタリング
            var filtered = stocks.AsEnumerable();

            if (SelectedLocation != null)
            {
                filtered = filtered.Where(s => s.LocationCode == SelectedLocation.LocationCode);
            }

            if (SelectedItem != null)
            {
                filtered = filtered.Where(s => s.ItemCode == SelectedItem.ItemCode);
            }

            if (!ShowZeroStock)
            {
                filtered = filtered.Where(s => s.StockQuantity != 0);
            }

            var filteredList = filtered.OrderBy(s => s.LocationCode).ThenBy(s => s.ItemCode).ToList();

            Stocks.Clear();
            foreach (var stock in filteredList)
            {
                Stocks.Add(stock);
            }

            // 合計を計算
            TotalStockQuantity = filteredList.Sum(s => s.StockQuantity);
            TotalPassedQuantity = filteredList.Sum(s => s.PassedQuantity);
            TotalDefectiveQuantity = filteredList.Sum(s => s.DefectiveQuantity);
            TotalUninspectedQuantity = filteredList.Sum(s => s.UninspectedQuantity);
        }
        catch (Exception ex)
        {
            await _dialogService.ShowErrorAsync("エラー", $"データの読み込みに失敗しました: {ex.Message}");
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
    private void ClearFilter()
    {
        SelectedLocation = null;
        SelectedItem = null;
        ShowZeroStock = false;
    }
}
