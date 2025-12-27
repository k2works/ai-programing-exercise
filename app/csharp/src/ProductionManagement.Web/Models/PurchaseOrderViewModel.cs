using System.ComponentModel.DataAnnotations;
using Microsoft.AspNetCore.Mvc.Rendering;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Web.Models;

/// <summary>
/// 発注一覧 ViewModel
/// </summary>
public class PurchaseOrderListViewModel
{
    public List<PurchaseOrderViewModel> PurchaseOrders { get; set; } = [];
    public PurchaseOrderSearchViewModel Search { get; set; } = new();
}

/// <summary>
/// 発注検索 ViewModel
/// </summary>
public class PurchaseOrderSearchViewModel
{
    [Display(Name = "キーワード")]
    public string? Keyword { get; set; }

    [Display(Name = "ステータス")]
    public string? Status { get; set; }

    [Display(Name = "発注日（開始）")]
    public DateOnly? OrderDateFrom { get; set; }

    [Display(Name = "発注日（終了）")]
    public DateOnly? OrderDateTo { get; set; }

    public List<SelectListItem> StatusOptions { get; set; } = [];
}

/// <summary>
/// 発注 ViewModel
/// </summary>
public class PurchaseOrderViewModel
{
    public int Id { get; set; }

    [Display(Name = "発注番号")]
    public string? PurchaseOrderNumber { get; set; }

    [Display(Name = "発注日")]
    [Required(ErrorMessage = "発注日は必須です")]
    public DateOnly OrderDate { get; set; } = DateOnly.FromDateTime(DateTime.Today);

    [Display(Name = "取引先コード")]
    [Required(ErrorMessage = "取引先は必須です")]
    public string? SupplierCode { get; set; }

    [Display(Name = "取引先名")]
    public string? SupplierName { get; set; }

    [Display(Name = "発注者コード")]
    public string? OrdererCode { get; set; }

    [Display(Name = "部門コード")]
    public string? DepartmentCode { get; set; }

    [Display(Name = "ステータス")]
    public PurchaseOrderStatus Status { get; set; } = PurchaseOrderStatus.Creating;

    [Display(Name = "ステータス")]
    public string StatusDisplayName => Status.GetDisplayName();

    [Display(Name = "備考")]
    [StringLength(500, ErrorMessage = "備考は500文字以内で入力してください")]
    public string? Remarks { get; set; }

    [Display(Name = "作成日時")]
    public DateTime? CreatedAt { get; set; }

    [Display(Name = "更新日時")]
    public DateTime? UpdatedAt { get; set; }

    public List<PurchaseOrderDetailViewModel> Details { get; set; } = [];

    /// <summary>
    /// 合計金額
    /// </summary>
    public decimal TotalAmount => Details.Sum(d => d.OrderAmount);

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static PurchaseOrderViewModel FromDomain(PurchaseOrder order)
    {
        return new PurchaseOrderViewModel
        {
            Id = order.Id,
            PurchaseOrderNumber = order.PurchaseOrderNumber,
            OrderDate = order.OrderDate,
            SupplierCode = order.SupplierCode,
            OrdererCode = order.OrdererCode,
            DepartmentCode = order.DepartmentCode,
            Status = order.Status,
            Remarks = order.Remarks,
            CreatedAt = order.CreatedAt,
            UpdatedAt = order.UpdatedAt,
            Details = order.Details.Select(PurchaseOrderDetailViewModel.FromDomain).ToList()
        };
    }
}

/// <summary>
/// 発注明細 ViewModel
/// </summary>
public class PurchaseOrderDetailViewModel
{
    public int Id { get; set; }

    [Display(Name = "発注番号")]
    public string? PurchaseOrderNumber { get; set; }

    [Display(Name = "行番号")]
    public int LineNumber { get; set; }

    [Display(Name = "品目コード")]
    [Required(ErrorMessage = "品目コードは必須です")]
    public string? ItemCode { get; set; }

    [Display(Name = "品目名")]
    public string? ItemName { get; set; }

    [Display(Name = "納入予定日")]
    [Required(ErrorMessage = "納入予定日は必須です")]
    public DateOnly ExpectedReceivingDate { get; set; } = DateOnly.FromDateTime(DateTime.Today.AddDays(7));

    [Display(Name = "確定納期")]
    public DateOnly? ConfirmedDeliveryDate { get; set; }

    [Display(Name = "単価")]
    [Required(ErrorMessage = "単価は必須です")]
    [Range(0, double.MaxValue, ErrorMessage = "単価は0以上で入力してください")]
    public decimal OrderUnitPrice { get; set; }

    [Display(Name = "発注数量")]
    [Required(ErrorMessage = "発注数量は必須です")]
    [Range(0.001, double.MaxValue, ErrorMessage = "発注数量は0より大きい値で入力してください")]
    public decimal OrderQuantity { get; set; }

    [Display(Name = "入荷数量")]
    public decimal ReceivedQuantity { get; set; }

    [Display(Name = "検収数量")]
    public decimal InspectedQuantity { get; set; }

    [Display(Name = "検収合格数量")]
    public decimal AcceptedQuantity { get; set; }

    [Display(Name = "金額")]
    public decimal OrderAmount { get; set; }

    [Display(Name = "税額")]
    public decimal TaxAmount { get; set; }

    [Display(Name = "完了")]
    public bool CompletedFlag { get; set; }

    [Display(Name = "明細備考")]
    [StringLength(200, ErrorMessage = "明細備考は200文字以内で入力してください")]
    public string? DetailRemarks { get; set; }

    /// <summary>
    /// 残数量（発注数量 - 入荷数量）
    /// </summary>
    public decimal RemainingQuantity => OrderQuantity - ReceivedQuantity;

    /// <summary>
    /// ドメインモデルから ViewModel を生成
    /// </summary>
    public static PurchaseOrderDetailViewModel FromDomain(PurchaseOrderDetail detail)
    {
        return new PurchaseOrderDetailViewModel
        {
            Id = detail.Id,
            PurchaseOrderNumber = detail.PurchaseOrderNumber,
            LineNumber = detail.LineNumber,
            ItemCode = detail.ItemCode,
            ExpectedReceivingDate = detail.ExpectedReceivingDate,
            ConfirmedDeliveryDate = detail.ConfirmedDeliveryDate,
            OrderUnitPrice = detail.OrderUnitPrice,
            OrderQuantity = detail.OrderQuantity,
            ReceivedQuantity = detail.ReceivedQuantity,
            InspectedQuantity = detail.InspectedQuantity,
            AcceptedQuantity = detail.AcceptedQuantity,
            OrderAmount = detail.OrderAmount,
            TaxAmount = detail.TaxAmount,
            CompletedFlag = detail.CompletedFlag,
            DetailRemarks = detail.DetailRemarks
        };
    }
}

/// <summary>
/// 発注登録 ViewModel
/// </summary>
public class PurchaseOrderCreateViewModel
{
    [Display(Name = "発注日")]
    [Required(ErrorMessage = "発注日は必須です")]
    public DateOnly OrderDate { get; set; } = DateOnly.FromDateTime(DateTime.Today);

    [Display(Name = "取引先")]
    [Required(ErrorMessage = "取引先は必須です")]
    public string? SupplierCode { get; set; }

    [Display(Name = "発注者コード")]
    public string? OrdererCode { get; set; }

    [Display(Name = "部門コード")]
    public string? DepartmentCode { get; set; }

    [Display(Name = "税率")]
    public decimal? TaxRate { get; set; } = 0.10m;

    [Display(Name = "備考")]
    [StringLength(500, ErrorMessage = "備考は500文字以内で入力してください")]
    public string? Remarks { get; set; }

    public List<PurchaseOrderDetailCreateViewModel> Details { get; set; } = [];

    public List<SelectListItem> SupplierOptions { get; set; } = [];
    public List<SelectListItem> ItemOptions { get; set; } = [];
}

/// <summary>
/// 発注明細登録 ViewModel
/// </summary>
public class PurchaseOrderDetailCreateViewModel
{
    [Display(Name = "品目コード")]
    [Required(ErrorMessage = "品目コードは必須です")]
    public string? ItemCode { get; set; }

    [Display(Name = "発注数量")]
    [Required(ErrorMessage = "発注数量は必須です")]
    [Range(0.001, double.MaxValue, ErrorMessage = "発注数量は0より大きい値で入力してください")]
    public decimal OrderQuantity { get; set; }

    [Display(Name = "納入予定日")]
    [Required(ErrorMessage = "納入予定日は必須です")]
    public DateOnly ExpectedReceivingDate { get; set; } = DateOnly.FromDateTime(DateTime.Today.AddDays(7));

    [Display(Name = "納入場所コード")]
    public string? DeliveryLocationCode { get; set; }
}
