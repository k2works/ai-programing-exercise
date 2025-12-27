using System.ComponentModel.DataAnnotations;
using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// 発注作成リクエスト
/// </summary>
public record CreatePurchaseOrderRequest(
    [Required(ErrorMessage = "取引先コードは必須です")]
    [StringLength(20, ErrorMessage = "取引先コードは20文字以内で入力してください")]
    string SupplierCode,

    [Required(ErrorMessage = "発注日は必須です")]
    DateOnly OrderDate,

    string? OrdererCode,
    string? DepartmentCode,
    decimal? TaxRate,
    string? Remarks,

    [Required(ErrorMessage = "発注明細は必須です")]
    [MinLength(1, ErrorMessage = "発注明細を1つ以上入力してください")]
    IReadOnlyList<CreatePurchaseOrderDetailRequest> Details
);

/// <summary>
/// 発注明細作成リクエスト
/// </summary>
public record CreatePurchaseOrderDetailRequest(
    [Required(ErrorMessage = "品目コードは必須です")]
    string ItemCode,

    [Required(ErrorMessage = "発注数量は必須です")]
    [Range(0.01, double.MaxValue, ErrorMessage = "発注数量は0より大きい値を入力してください")]
    decimal OrderQuantity,

    [Required(ErrorMessage = "入荷予定日は必須です")]
    DateOnly ExpectedReceivingDate,

    string? OrderNumber,
    string? DeliveryLocationCode
);

/// <summary>
/// 発注レスポンス
/// </summary>
public record PurchaseOrderResponse(
    int Id,
    string PurchaseOrderNumber,
    DateOnly OrderDate,
    string SupplierCode,
    string? OrdererCode,
    string? DepartmentCode,
    string Status,
    string? Remarks,
    IReadOnlyList<PurchaseOrderDetailResponse> Details
)
{
    public static PurchaseOrderResponse From(PurchaseOrder order) => new(
        Id: order.Id,
        PurchaseOrderNumber: order.PurchaseOrderNumber,
        OrderDate: order.OrderDate,
        SupplierCode: order.SupplierCode,
        OrdererCode: order.OrdererCode,
        DepartmentCode: order.DepartmentCode,
        Status: order.Status.GetDisplayName(),
        Remarks: order.Remarks,
        Details: order.Details.Select(PurchaseOrderDetailResponse.From).ToList()
    );

    public static PurchaseOrderResponse FromWithoutDetails(PurchaseOrder order) => new(
        Id: order.Id,
        PurchaseOrderNumber: order.PurchaseOrderNumber,
        OrderDate: order.OrderDate,
        SupplierCode: order.SupplierCode,
        OrdererCode: order.OrdererCode,
        DepartmentCode: order.DepartmentCode,
        Status: order.Status.GetDisplayName(),
        Remarks: order.Remarks,
        Details: []
    );
}

/// <summary>
/// 発注明細レスポンス
/// </summary>
public record PurchaseOrderDetailResponse(
    int Id,
    int LineNumber,
    string? OrderNumber,
    string? DeliveryLocationCode,
    string ItemCode,
    bool MiscellaneousItemFlag,
    DateOnly ExpectedReceivingDate,
    DateOnly? ConfirmedDeliveryDate,
    decimal OrderUnitPrice,
    decimal OrderQuantity,
    decimal ReceivedQuantity,
    decimal InspectedQuantity,
    decimal AcceptedQuantity,
    decimal OrderAmount,
    decimal TaxAmount,
    bool CompletedFlag,
    string? DetailRemarks
)
{
    public static PurchaseOrderDetailResponse From(PurchaseOrderDetail detail) => new(
        Id: detail.Id,
        LineNumber: detail.LineNumber,
        OrderNumber: detail.OrderNumber,
        DeliveryLocationCode: detail.DeliveryLocationCode,
        ItemCode: detail.ItemCode,
        MiscellaneousItemFlag: detail.MiscellaneousItemFlag,
        ExpectedReceivingDate: detail.ExpectedReceivingDate,
        ConfirmedDeliveryDate: detail.ConfirmedDeliveryDate,
        OrderUnitPrice: detail.OrderUnitPrice,
        OrderQuantity: detail.OrderQuantity,
        ReceivedQuantity: detail.ReceivedQuantity,
        InspectedQuantity: detail.InspectedQuantity,
        AcceptedQuantity: detail.AcceptedQuantity,
        OrderAmount: detail.OrderAmount,
        TaxAmount: detail.TaxAmount,
        CompletedFlag: detail.CompletedFlag,
        DetailRemarks: detail.DetailRemarks
    );
}
