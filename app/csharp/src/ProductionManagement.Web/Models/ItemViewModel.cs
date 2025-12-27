using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Domain.Models.Item;

namespace ProductionManagement.Web.Models;

/// <summary>
/// 品目一覧 ViewModel
/// </summary>
public class ItemListViewModel
{
    public List<ItemViewModel> Items { get; set; } = [];
    public ItemSearchViewModel Search { get; set; } = new();
}

/// <summary>
/// 品目検索 ViewModel
/// </summary>
public class ItemSearchViewModel
{
    [Display(Name = "キーワード")]
    public string? Keyword { get; set; }

    [Display(Name = "品目区分")]
    public string? Category { get; set; }

    public List<SelectListItem> CategoryOptions { get; set; } = [];
}

/// <summary>
/// 品目 ViewModel
/// </summary>
public class ItemViewModel
{
    public int Id { get; set; }

    [Display(Name = "品目コード")]
    [Required(ErrorMessage = "品目コードは必須です")]
    [StringLength(20, ErrorMessage = "品目コードは20文字以内で入力してください")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    [Required(ErrorMessage = "品目名は必須です")]
    [StringLength(100, ErrorMessage = "品目名は100文字以内で入力してください")]
    public string? ItemName { get; set; }

    [Display(Name = "品目区分")]
    public ItemCategory ItemCategory { get; set; } = ItemCategory.Product;

    [Display(Name = "品目区分")]
    public string ItemCategoryDisplayName => ItemCategory.GetDisplayName();

    [Display(Name = "単位")]
    [StringLength(10, ErrorMessage = "単位は10文字以内で入力してください")]
    public string? UnitCode { get; set; }

    [Display(Name = "リードタイム（日）")]
    [Range(0, 365, ErrorMessage = "リードタイムは0〜365の範囲で入力してください")]
    public int LeadTime { get; set; }

    [Display(Name = "安全リードタイム（日）")]
    [Range(0, 365, ErrorMessage = "安全リードタイムは0〜365の範囲で入力してください")]
    public int SafetyLeadTime { get; set; }

    [Display(Name = "安全在庫")]
    [Range(0, double.MaxValue, ErrorMessage = "安全在庫は0以上で入力してください")]
    public decimal SafetyStock { get; set; }

    [Display(Name = "歩留率（%）")]
    [Range(0, 100, ErrorMessage = "歩留率は0〜100の範囲で入力してください")]
    public decimal YieldRate { get; set; } = 100m;

    [Display(Name = "最小ロットサイズ")]
    [Range(0.001, double.MaxValue, ErrorMessage = "最小ロットサイズは0より大きい値で入力してください")]
    public decimal MinLotSize { get; set; } = 1m;

    [Display(Name = "ロット増分")]
    [Range(0.001, double.MaxValue, ErrorMessage = "ロット増分は0より大きい値で入力してください")]
    public decimal LotIncrement { get; set; } = 1m;

    [Display(Name = "最大ロットサイズ")]
    [Range(0, double.MaxValue, ErrorMessage = "最大ロットサイズは0以上で入力してください")]
    public decimal? MaxLotSize { get; set; }

    [Display(Name = "有効期限（日）")]
    [Range(1, 3650, ErrorMessage = "有効期限は1〜3650の範囲で入力してください")]
    public int? ShelfLife { get; set; }

    [Display(Name = "有効開始日")]
    public DateOnly EffectiveFrom { get; set; }

    [Display(Name = "有効終了日")]
    public DateOnly? EffectiveTo { get; set; }

    [Display(Name = "作成日時")]
    public DateTime CreatedAt { get; set; }

    [Display(Name = "更新日時")]
    public DateTime UpdatedAt { get; set; }

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static ItemViewModel FromDomain(Item item)
    {
        return new ItemViewModel
        {
            Id = item.Id,
            ItemCode = item.ItemCode,
            ItemName = item.ItemName,
            ItemCategory = item.ItemCategory,
            UnitCode = item.UnitCode,
            LeadTime = item.LeadTime,
            SafetyLeadTime = item.SafetyLeadTime,
            SafetyStock = item.SafetyStock,
            YieldRate = item.YieldRate,
            MinLotSize = item.MinLotSize,
            LotIncrement = item.LotIncrement,
            MaxLotSize = item.MaxLotSize,
            ShelfLife = item.ShelfLife,
            EffectiveFrom = item.EffectiveFrom,
            EffectiveTo = item.EffectiveTo,
            CreatedAt = item.CreatedAt,
            UpdatedAt = item.UpdatedAt
        };
    }
}
