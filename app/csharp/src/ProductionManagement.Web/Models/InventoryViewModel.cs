using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Application.Port.In;
using ProductionManagement.Domain.Models.Inventory;

namespace ProductionManagement.Web.Models;

/// <summary>
/// 在庫一覧 ViewModel
/// </summary>
public class InventoryListViewModel
{
    public List<StockViewModel> Stocks { get; set; } = [];
    public List<InventorySummaryViewModel> Summaries { get; set; } = [];
    public InventorySearchViewModel Search { get; set; } = new();
}

/// <summary>
/// 在庫検索 ViewModel
/// </summary>
public class InventorySearchViewModel
{
    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    [Display(Name = "場所コード")]
    public string? LocationCode { get; set; }

    [Display(Name = "在庫状態")]
    public string? StockState { get; set; }

    public List<SelectListItem> StockStateOptions { get; set; } = [];
}

/// <summary>
/// 在庫 ViewModel
/// </summary>
public class StockViewModel
{
    public long? Id { get; set; }

    [Display(Name = "場所コード")]
    public string? LocationCode { get; set; }

    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    public string? ItemName { get; set; }

    [Display(Name = "在庫数量")]
    public decimal StockQuantity { get; set; }

    [Display(Name = "合格数量")]
    public decimal PassedQuantity { get; set; }

    [Display(Name = "不良数量")]
    public decimal DefectiveQuantity { get; set; }

    [Display(Name = "未検査数量")]
    public decimal UninspectedQuantity { get; set; }

    [Display(Name = "更新日時")]
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// 有効在庫（合格数量のみ）
    /// </summary>
    public decimal AvailableQuantity => PassedQuantity;

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static StockViewModel FromDomain(Stock stock)
    {
        return new StockViewModel
        {
            Id = stock.Id,
            LocationCode = stock.LocationCode,
            ItemCode = stock.ItemCode,
            StockQuantity = stock.StockQuantity,
            PassedQuantity = stock.PassedQuantity,
            DefectiveQuantity = stock.DefectiveQuantity,
            UninspectedQuantity = stock.UninspectedQuantity,
            UpdatedAt = stock.UpdatedAt
        };
    }
}

/// <summary>
/// 在庫サマリー ViewModel
/// </summary>
public class InventorySummaryViewModel
{
    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    public string? ItemName { get; set; }

    [Display(Name = "総在庫数量")]
    public decimal TotalQuantity { get; set; }

    [Display(Name = "安全在庫")]
    public decimal? SafetyStock { get; set; }

    [Display(Name = "在庫状態")]
    public StockState StockState { get; set; }

    [Display(Name = "在庫状態")]
    public string StockStateDisplayName => StockState switch
    {
        StockState.Normal => "正常",
        StockState.Shortage => "不足",
        StockState.Excess => "過剰",
        _ => "不明"
    };

    /// <summary>
    /// 過不足数量（安全在庫との差分）
    /// </summary>
    public decimal? DifferenceQuantity => SafetyStock.HasValue ? TotalQuantity - SafetyStock.Value : null;

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static InventorySummaryViewModel FromDomain(InventorySummary summary)
    {
        return new InventorySummaryViewModel
        {
            ItemCode = summary.ItemCode,
            ItemName = summary.ItemName,
            TotalQuantity = summary.TotalQuantity,
            SafetyStock = summary.SafetyStock,
            StockState = summary.StockState
        };
    }
}
