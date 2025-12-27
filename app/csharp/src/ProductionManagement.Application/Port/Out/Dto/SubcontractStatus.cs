using ProductionManagement.Domain.Models.Purchase;

namespace ProductionManagement.Application.Port.Out.Dto;

/// <summary>
/// 外注委託状況DTO
/// </summary>
public class SubcontractStatus
{
    public required string PurchaseOrderNumber { get; init; }
    public PurchaseOrderStatus Status { get; init; }
    public decimal SuppliedQuantity { get; init; }
    public decimal ConsumedQuantity { get; init; }
    public decimal AcceptedQuantity { get; init; }
    public decimal YieldRate { get; init; }
}
