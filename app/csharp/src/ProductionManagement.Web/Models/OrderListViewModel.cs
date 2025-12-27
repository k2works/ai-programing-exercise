using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Web.Models;

/// <summary>
/// オーダ一覧 ViewModel
/// </summary>
public class OrderListViewModel
{
    public List<OrderViewModel> Orders { get; set; } = [];
    public OrderSearchViewModel Search { get; set; } = new();
}

/// <summary>
/// オーダ検索 ViewModel
/// </summary>
public class OrderSearchViewModel
{
    [Display(Name = "キーワード")]
    public string? Keyword { get; set; }

    [Display(Name = "オーダ種別")]
    public string? OrderType { get; set; }

    [Display(Name = "ステータス")]
    public string? Status { get; set; }

    [Display(Name = "納期（開始）")]
    public DateOnly? DueDateFrom { get; set; }

    [Display(Name = "納期（終了）")]
    public DateOnly? DueDateTo { get; set; }

    public List<SelectListItem> OrderTypeOptions { get; set; } = [];
    public List<SelectListItem> StatusOptions { get; set; } = [];
}

/// <summary>
/// オーダ登録 ViewModel
/// </summary>
public class OrderCreateViewModel
{
    [Display(Name = "オーダ種別")]
    [Required(ErrorMessage = "オーダ種別は必須です")]
    public OrderType OrderType { get; set; } = OrderType.Manufacturing;

    [Display(Name = "品目コード")]
    [Required(ErrorMessage = "品目は必須です")]
    public string? ItemCode { get; set; }

    [Display(Name = "着手日")]
    [Required(ErrorMessage = "着手日は必須です")]
    public DateOnly StartDate { get; set; } = DateOnly.FromDateTime(DateTime.Today);

    [Display(Name = "納期")]
    [Required(ErrorMessage = "納期は必須です")]
    public DateOnly DueDate { get; set; } = DateOnly.FromDateTime(DateTime.Today.AddDays(7));

    [Display(Name = "計画数量")]
    [Required(ErrorMessage = "計画数量は必須です")]
    [Range(0.001, double.MaxValue, ErrorMessage = "計画数量は0より大きい値で入力してください")]
    public decimal PlanQuantity { get; set; }

    [Display(Name = "場所コード")]
    [Required(ErrorMessage = "場所コードは必須です")]
    [StringLength(20, ErrorMessage = "場所コードは20文字以内で入力してください")]
    public string? LocationCode { get; set; } = "MAIN";

    [Display(Name = "有効期限")]
    public DateOnly? ExpirationDate { get; set; }

    public List<SelectListItem> OrderTypeOptions { get; set; } = [];
    public List<SelectListItem> ItemOptions { get; set; } = [];
}
