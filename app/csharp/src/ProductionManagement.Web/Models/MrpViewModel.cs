using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Web.Models;

/// <summary>
/// MRP 実行 ViewModel
/// </summary>
public class MrpExecuteViewModel
{
    [Display(Name = "オーダID")]
    [Required(ErrorMessage = "オーダIDは必須です")]
    public int? OrderId { get; set; }

    public List<SelectListItem> OrderOptions { get; set; } = [];
}

/// <summary>
/// MRP 結果 ViewModel
/// </summary>
public class MrpResultViewModel
{
    public OrderViewModel Order { get; set; } = new();
    public List<RequirementViewModel> Requirements { get; set; } = [];

    /// <summary>
    /// 総所要量
    /// </summary>
    public decimal TotalRequiredQuantity => Requirements.Sum(r => r.RequiredQuantity);

    /// <summary>
    /// 総不足量
    /// </summary>
    public decimal TotalShortageQuantity => Requirements.Sum(r => r.ShortageQuantity);
}

/// <summary>
/// オーダ ViewModel
/// </summary>
public class OrderViewModel
{
    public int Id { get; set; }

    [Display(Name = "オーダ番号")]
    public string? OrderNumber { get; set; }

    [Display(Name = "オーダ種別")]
    public OrderType OrderType { get; set; }

    [Display(Name = "オーダ種別")]
    public string OrderTypeDisplayName => OrderType.GetDisplayName();

    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    public string? ItemName { get; set; }

    [Display(Name = "着手日")]
    public DateOnly StartDate { get; set; }

    [Display(Name = "納期")]
    public DateOnly DueDate { get; set; }

    [Display(Name = "計画数量")]
    public decimal PlanQuantity { get; set; }

    [Display(Name = "ロケーション")]
    public string? LocationCode { get; set; }

    [Display(Name = "ステータス")]
    public PlanStatus Status { get; set; }

    [Display(Name = "ステータス")]
    public string StatusDisplayName => Status.GetDisplayName();

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static OrderViewModel FromDomain(Order order)
    {
        return new OrderViewModel
        {
            Id = order.Id,
            OrderNumber = order.OrderNumber,
            OrderType = order.OrderType,
            ItemCode = order.ItemCode,
            ItemName = order.Item?.ItemName,
            StartDate = order.StartDate,
            DueDate = order.DueDate,
            PlanQuantity = order.PlanQuantity,
            LocationCode = order.LocationCode,
            Status = order.Status
        };
    }
}

/// <summary>
/// 所要 ViewModel
/// </summary>
public class RequirementViewModel
{
    public int Id { get; set; }

    [Display(Name = "所要番号")]
    public string? RequirementNumber { get; set; }

    [Display(Name = "オーダID")]
    public int OrderId { get; set; }

    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    public string? ItemName { get; set; }

    [Display(Name = "納期")]
    public DateOnly DueDate { get; set; }

    [Display(Name = "所要量")]
    public decimal RequiredQuantity { get; set; }

    [Display(Name = "引当量")]
    public decimal AllocatedQuantity { get; set; }

    [Display(Name = "不足量")]
    public decimal ShortageQuantity { get; set; }

    [Display(Name = "ロケーション")]
    public string? LocationCode { get; set; }

    /// <summary>
    /// 引当率（%）
    /// </summary>
    public decimal AllocationRate => RequiredQuantity > 0
        ? Math.Round(AllocatedQuantity / RequiredQuantity * 100, 1)
        : 0;

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static RequirementViewModel FromDomain(Requirement requirement)
    {
        return new RequirementViewModel
        {
            Id = requirement.Id,
            RequirementNumber = requirement.RequirementNumber,
            OrderId = requirement.OrderId,
            ItemCode = requirement.ItemCode,
            ItemName = requirement.Item?.ItemName,
            DueDate = requirement.DueDate,
            RequiredQuantity = requirement.RequiredQuantity,
            AllocatedQuantity = requirement.AllocatedQuantity,
            ShortageQuantity = requirement.ShortageQuantity,
            LocationCode = requirement.LocationCode
        };
    }
}
