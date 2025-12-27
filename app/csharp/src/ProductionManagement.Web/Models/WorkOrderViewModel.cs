using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Web.Models;

/// <summary>
/// 作業指示一覧 ViewModel
/// </summary>
public class WorkOrderListViewModel
{
    public List<WorkOrderViewModel> WorkOrders { get; set; } = [];
    public WorkOrderSearchViewModel Search { get; set; } = new();
}

/// <summary>
/// 作業指示検索 ViewModel
/// </summary>
public class WorkOrderSearchViewModel
{
    [Display(Name = "キーワード")]
    public string? Keyword { get; set; }

    [Display(Name = "ステータス")]
    public string? Status { get; set; }

    [Display(Name = "作業指示日（開始）")]
    public DateOnly? WorkOrderDateFrom { get; set; }

    [Display(Name = "作業指示日（終了）")]
    public DateOnly? WorkOrderDateTo { get; set; }

    public List<SelectListItem> StatusOptions { get; set; } = [];
}

/// <summary>
/// 作業指示 ViewModel
/// </summary>
public class WorkOrderViewModel
{
    public int Id { get; set; }

    [Display(Name = "作業指示番号")]
    public string? WorkOrderNumber { get; set; }

    [Display(Name = "オーダ番号")]
    public string? OrderNumber { get; set; }

    [Display(Name = "作業指示日")]
    public DateOnly WorkOrderDate { get; set; }

    [Display(Name = "品目コード")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    public string? ItemName { get; set; }

    [Display(Name = "指示数量")]
    public decimal OrderQuantity { get; set; }

    [Display(Name = "場所コード")]
    public string? LocationCode { get; set; }

    [Display(Name = "着手予定日")]
    public DateOnly PlannedStartDate { get; set; }

    [Display(Name = "終了予定日")]
    public DateOnly PlannedEndDate { get; set; }

    [Display(Name = "着手実績日")]
    public DateOnly? ActualStartDate { get; set; }

    [Display(Name = "終了実績日")]
    public DateOnly? ActualEndDate { get; set; }

    [Display(Name = "完成数量")]
    public decimal CompletedQuantity { get; set; }

    [Display(Name = "良品数量")]
    public decimal TotalGoodQuantity { get; set; }

    [Display(Name = "不良数量")]
    public decimal TotalDefectQuantity { get; set; }

    [Display(Name = "ステータス")]
    public WorkOrderStatus Status { get; set; }

    [Display(Name = "ステータス")]
    public string StatusDisplayName => Status.GetDisplayName();

    [Display(Name = "完了")]
    public bool CompletedFlag { get; set; }

    [Display(Name = "備考")]
    public string? Remarks { get; set; }

    [Display(Name = "作成日時")]
    public DateTime? CreatedAt { get; set; }

    [Display(Name = "更新日時")]
    public DateTime? UpdatedAt { get; set; }

    /// <summary>
    /// 進捗率（%）
    /// </summary>
    public decimal ProgressRate => OrderQuantity > 0
        ? Math.Round(CompletedQuantity / OrderQuantity * 100, 1)
        : 0;

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static WorkOrderViewModel FromDomain(WorkOrder workOrder)
    {
        return new WorkOrderViewModel
        {
            Id = workOrder.Id,
            WorkOrderNumber = workOrder.WorkOrderNumber,
            OrderNumber = workOrder.OrderNumber,
            WorkOrderDate = workOrder.WorkOrderDate,
            ItemCode = workOrder.ItemCode,
            OrderQuantity = workOrder.OrderQuantity,
            LocationCode = workOrder.LocationCode,
            PlannedStartDate = workOrder.PlannedStartDate,
            PlannedEndDate = workOrder.PlannedEndDate,
            ActualStartDate = workOrder.ActualStartDate,
            ActualEndDate = workOrder.ActualEndDate,
            CompletedQuantity = workOrder.CompletedQuantity,
            TotalGoodQuantity = workOrder.TotalGoodQuantity,
            TotalDefectQuantity = workOrder.TotalDefectQuantity,
            Status = workOrder.Status,
            CompletedFlag = workOrder.CompletedFlag,
            Remarks = workOrder.Remarks,
            CreatedAt = workOrder.CreatedAt,
            UpdatedAt = workOrder.UpdatedAt
        };
    }
}

/// <summary>
/// 作業指示登録 ViewModel
/// </summary>
public class WorkOrderCreateViewModel
{
    [Display(Name = "オーダ番号")]
    [Required(ErrorMessage = "オーダ番号は必須です")]
    public string? OrderNumber { get; set; }

    [Display(Name = "作業指示日")]
    [Required(ErrorMessage = "作業指示日は必須です")]
    public DateOnly WorkOrderDate { get; set; } = DateOnly.FromDateTime(DateTime.Today);

    [Display(Name = "場所コード")]
    [Required(ErrorMessage = "場所コードは必須です")]
    [StringLength(20, ErrorMessage = "場所コードは20文字以内で入力してください")]
    public string? LocationCode { get; set; } = "MAIN";

    [Display(Name = "着手予定日")]
    [Required(ErrorMessage = "着手予定日は必須です")]
    public DateOnly PlannedStartDate { get; set; } = DateOnly.FromDateTime(DateTime.Today);

    [Display(Name = "終了予定日")]
    [Required(ErrorMessage = "終了予定日は必須です")]
    public DateOnly PlannedEndDate { get; set; } = DateOnly.FromDateTime(DateTime.Today.AddDays(1));

    [Display(Name = "備考")]
    [StringLength(500, ErrorMessage = "備考は500文字以内で入力してください")]
    public string? Remarks { get; set; }

    public List<SelectListItem> OrderOptions { get; set; } = [];
}
