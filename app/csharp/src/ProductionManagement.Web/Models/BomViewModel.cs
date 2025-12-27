using System.ComponentModel.DataAnnotations;
using ProductionManagement.Application.Services;

namespace ProductionManagement.Web.Models;

/// <summary>
/// BOM 展開 ViewModel
/// </summary>
public class BomExplodeViewModel
{
    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    public BomNodeViewModel? BomTree { get; set; }
}

/// <summary>
/// BOM ノード ViewModel
/// </summary>
public class BomNodeViewModel
{
    public string ItemCode { get; set; } = string.Empty;
    public string ItemName { get; set; } = string.Empty;
    public decimal RequiredQuantity { get; set; }
    public int Level { get; set; }
    public List<BomNodeViewModel> Children { get; set; } = [];

    public static BomNodeViewModel FromDomain(BomNode node)
    {
        return new BomNodeViewModel
        {
            ItemCode = node.ItemCode,
            ItemName = node.ItemName,
            RequiredQuantity = node.RequiredQuantity,
            Level = node.Level,
            Children = node.Children.Select(FromDomain).ToList()
        };
    }
}

/// <summary>
/// 使用先照会 ViewModel
/// </summary>
public class WhereUsedViewModel
{
    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    public List<WhereUsedResultViewModel> Results { get; set; } = [];
}

/// <summary>
/// 使用先照会結果 ViewModel
/// </summary>
public class WhereUsedResultViewModel
{
    public string ParentItemCode { get; set; } = string.Empty;
    public string ItemName { get; set; } = string.Empty;
    public decimal RequiredQuantity { get; set; }

    public static WhereUsedResultViewModel FromDomain(WhereUsedResult result)
    {
        return new WhereUsedResultViewModel
        {
            ParentItemCode = result.ParentItemCode,
            ItemName = result.ItemName,
            RequiredQuantity = result.RequiredQuantity
        };
    }
}
