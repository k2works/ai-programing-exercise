using System.ComponentModel.DataAnnotations;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// オーダ登録リクエスト
/// </summary>
public record CreateOrderRequest(
    [Required(ErrorMessage = "オーダ種別は必須です")]
    string OrderType,

    [Required(ErrorMessage = "品目コードは必須です")]
    string ItemCode,

    [Required(ErrorMessage = "着手日は必須です")]
    DateOnly StartDate,

    [Required(ErrorMessage = "納期は必須です")]
    DateOnly DueDate,

    [Required(ErrorMessage = "計画数量は必須です")]
    [Range(0.01, double.MaxValue, ErrorMessage = "計画数量は0より大きい値を入力してください")]
    decimal PlanQuantity,

    [Required(ErrorMessage = "場所コードは必須です")]
    string LocationCode,

    int? MpsId = null,
    int? ParentOrderId = null,
    DateOnly? ExpirationDate = null
);

/// <summary>
/// オーダレスポンス
/// </summary>
public record OrderResponse(
    int Id,
    string OrderNumber,
    string OrderType,
    string ItemCode,
    DateOnly StartDate,
    DateOnly DueDate,
    DateOnly? ExpirationDate,
    decimal PlanQuantity,
    string LocationCode,
    string Status,
    int? MpsId,
    int? ParentOrderId
)
{
    public static OrderResponse From(Order order) => new(
        Id: order.Id,
        OrderNumber: order.OrderNumber,
        OrderType: order.OrderType.GetDisplayName(),
        ItemCode: order.ItemCode,
        StartDate: order.StartDate,
        DueDate: order.DueDate,
        ExpirationDate: order.ExpirationDate,
        PlanQuantity: order.PlanQuantity,
        LocationCode: order.LocationCode,
        Status: order.Status.GetDisplayName(),
        MpsId: order.MpsId,
        ParentOrderId: order.ParentOrderId
    );
}
