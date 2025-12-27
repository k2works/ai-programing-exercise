using System.ComponentModel.DataAnnotations;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Infrastructure.Rest.Dto;

/// <summary>
/// MRP 実行リクエスト
/// </summary>
public record MrpExecuteRequest(
    [Required(ErrorMessage = "オーダIDは必須です")]
    int OrderId
);

/// <summary>
/// 在庫引当リクエスト
/// </summary>
public record AllocateInventoryRequest(
    [Required(ErrorMessage = "所要量IDは必須です")]
    int RequirementId,

    [Required(ErrorMessage = "引当数量は必須です")]
    [Range(0.01, double.MaxValue, ErrorMessage = "引当数量は0より大きい値を入力してください")]
    decimal InventoryQuantity
);

/// <summary>
/// 所要量レスポンス
/// </summary>
public record RequirementResponse(
    int Id,
    string RequirementNumber,
    int OrderId,
    string ItemCode,
    DateOnly DueDate,
    decimal RequiredQuantity,
    decimal AllocatedQuantity,
    decimal ShortageQuantity,
    string LocationCode
)
{
    public static RequirementResponse From(Requirement requirement) => new(
        Id: requirement.Id,
        RequirementNumber: requirement.RequirementNumber,
        OrderId: requirement.OrderId,
        ItemCode: requirement.ItemCode,
        DueDate: requirement.DueDate,
        RequiredQuantity: requirement.RequiredQuantity,
        AllocatedQuantity: requirement.AllocatedQuantity,
        ShortageQuantity: requirement.ShortageQuantity,
        LocationCode: requirement.LocationCode
    );
}

/// <summary>
/// 引当レスポンス
/// </summary>
public record AllocationResponse(
    int Id,
    int RequirementId,
    string AllocationType,
    DateOnly AllocationDate,
    decimal AllocatedQuantity,
    string LocationCode
)
{
    public static AllocationResponse From(Allocation allocation) => new(
        Id: allocation.Id,
        RequirementId: allocation.RequirementId,
        AllocationType: allocation.AllocationType.ToString(),
        AllocationDate: allocation.AllocationDate,
        AllocatedQuantity: allocation.AllocatedQuantity,
        LocationCode: allocation.LocationCode
    );
}
