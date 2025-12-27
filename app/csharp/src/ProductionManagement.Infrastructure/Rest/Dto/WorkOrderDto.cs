using System.ComponentModel.DataAnnotations;
using ProductionManagement.Domain.Models.Process;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// 作業指示作成リクエスト
/// </summary>
public record CreateWorkOrderRequest(
    [Required(ErrorMessage = "オーダ番号は必須です")]
    string OrderNumber,

    [Required(ErrorMessage = "作業指示日は必須です")]
    DateOnly WorkOrderDate,

    [Required(ErrorMessage = "場所コードは必須です")]
    string LocationCode,

    [Required(ErrorMessage = "開始予定日は必須です")]
    DateOnly PlannedStartDate,

    [Required(ErrorMessage = "完了予定日は必須です")]
    DateOnly PlannedEndDate,

    string? Remarks
);

/// <summary>
/// 作業指示レスポンス
/// </summary>
public record WorkOrderResponse(
    int Id,
    string WorkOrderNumber,
    string OrderNumber,
    DateOnly WorkOrderDate,
    string ItemCode,
    decimal OrderQuantity,
    string LocationCode,
    DateOnly PlannedStartDate,
    DateOnly PlannedEndDate,
    DateOnly? ActualStartDate,
    DateOnly? ActualEndDate,
    decimal CompletedQuantity,
    decimal TotalGoodQuantity,
    decimal TotalDefectQuantity,
    string Status,
    bool CompletedFlag,
    string? Remarks,
    IReadOnlyList<WorkOrderDetailResponse> Details
)
{
    public static WorkOrderResponse From(WorkOrder workOrder) => new(
        Id: workOrder.Id,
        WorkOrderNumber: workOrder.WorkOrderNumber,
        OrderNumber: workOrder.OrderNumber,
        WorkOrderDate: workOrder.WorkOrderDate,
        ItemCode: workOrder.ItemCode,
        OrderQuantity: workOrder.OrderQuantity,
        LocationCode: workOrder.LocationCode,
        PlannedStartDate: workOrder.PlannedStartDate,
        PlannedEndDate: workOrder.PlannedEndDate,
        ActualStartDate: workOrder.ActualStartDate,
        ActualEndDate: workOrder.ActualEndDate,
        CompletedQuantity: workOrder.CompletedQuantity,
        TotalGoodQuantity: workOrder.TotalGoodQuantity,
        TotalDefectQuantity: workOrder.TotalDefectQuantity,
        Status: workOrder.Status.GetDisplayName(),
        CompletedFlag: workOrder.CompletedFlag,
        Remarks: workOrder.Remarks,
        Details: workOrder.Details.Select(WorkOrderDetailResponse.From).ToList()
    );

    public static WorkOrderResponse FromWithoutDetails(WorkOrder workOrder) => new(
        Id: workOrder.Id,
        WorkOrderNumber: workOrder.WorkOrderNumber,
        OrderNumber: workOrder.OrderNumber,
        WorkOrderDate: workOrder.WorkOrderDate,
        ItemCode: workOrder.ItemCode,
        OrderQuantity: workOrder.OrderQuantity,
        LocationCode: workOrder.LocationCode,
        PlannedStartDate: workOrder.PlannedStartDate,
        PlannedEndDate: workOrder.PlannedEndDate,
        ActualStartDate: workOrder.ActualStartDate,
        ActualEndDate: workOrder.ActualEndDate,
        CompletedQuantity: workOrder.CompletedQuantity,
        TotalGoodQuantity: workOrder.TotalGoodQuantity,
        TotalDefectQuantity: workOrder.TotalDefectQuantity,
        Status: workOrder.Status.GetDisplayName(),
        CompletedFlag: workOrder.CompletedFlag,
        Remarks: workOrder.Remarks,
        Details: []
    );
}

/// <summary>
/// 作業指示明細レスポンス
/// </summary>
public record WorkOrderDetailResponse(
    int Id,
    int Sequence,
    string ProcessCode
)
{
    public static WorkOrderDetailResponse From(WorkOrderDetail detail) => new(
        Id: detail.Id,
        Sequence: detail.Sequence,
        ProcessCode: detail.ProcessCode
    );
}
