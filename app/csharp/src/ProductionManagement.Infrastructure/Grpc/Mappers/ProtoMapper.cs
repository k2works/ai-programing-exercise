using ProductionManagement.Application.Port.In;
using ProductionManagement.Application.Port.In.Command;
using ProductionManagement.Application.Services;
using ProductionManagement.Domain.Models.Plan;
using ProductionManagement.Domain.Models.Purchase;
using ProductionManagement.Grpc.Protos;
using DomainItem = ProductionManagement.Domain.Models.Item.Item;
using DomainItemCategory = ProductionManagement.Domain.Models.Item.ItemCategory;
using DomainPurchaseOrderStatus = ProductionManagement.Domain.Models.Purchase.PurchaseOrderStatus;
using DomainOrderType = ProductionManagement.Domain.Models.Plan.OrderType;
using DomainPlanStatus = ProductionManagement.Domain.Models.Plan.PlanStatus;
using ProtoItemCategory = ProductionManagement.Grpc.Protos.ItemCategory;
using ProtoPurchaseOrderStatus = ProductionManagement.Grpc.Protos.PurchaseOrderStatus;
using ProtoOrderType = ProductionManagement.Grpc.Protos.OrderType;
using ProtoPlanStatus = ProductionManagement.Grpc.Protos.PlanStatus;
using ProtoDecimal = ProductionManagement.Grpc.Protos.Decimal;

namespace ProductionManagement.Infrastructure.Grpc.Mappers;

/// <summary>
/// ドメインモデルと Protocol Buffers メッセージ間の変換
/// </summary>
public class ProtoMapper
{
    // ========== Item ==========

    /// <summary>
    /// ドメインモデルを Proto メッセージに変換
    /// </summary>
    public ItemMessage ToProto(DomainItem domain)
    {
        var message = new ItemMessage
        {
            Id = domain.Id,
            ItemCode = domain.ItemCode,
            EffectiveFrom = ToProtoDate(domain.EffectiveFrom),
            ItemName = domain.ItemName,
            Category = ToProtoCategory(domain.ItemCategory),
            UnitCode = domain.UnitCode ?? "",
            LeadTime = domain.LeadTime,
            SafetyLeadTime = domain.SafetyLeadTime,
            SafetyStock = ToProtoDecimal(domain.SafetyStock),
            YieldRate = ToProtoDecimal(domain.YieldRate),
            MinLotSize = ToProtoDecimal(domain.MinLotSize),
            LotIncrement = ToProtoDecimal(domain.LotIncrement),
        };

        if (domain.EffectiveTo.HasValue)
        {
            message.EffectiveTo = ToProtoDate(domain.EffectiveTo.Value);
        }

        if (domain.MaxLotSize.HasValue)
        {
            message.MaxLotSize = ToProtoDecimal(domain.MaxLotSize.Value);
        }

        if (domain.ShelfLife.HasValue)
        {
            message.ShelfLife = domain.ShelfLife.Value;
        }

        return message;
    }

    /// <summary>
    /// CreateItemRequest を CreateItemCommand に変換
    /// </summary>
    public CreateItemCommand ToCreateCommand(CreateItemRequest request)
    {
        return new CreateItemCommand(
            ItemCode: request.ItemCode,
            ItemName: request.ItemName,
            Category: ToDomainCategory(request.Category),
            UnitCode: EmptyToNull(request.UnitCode),
            LeadTime: request.LeadTime,
            SafetyLeadTime: request.SafetyLeadTime,
            SafetyStock: ToDomainDecimal(request.SafetyStock) ?? 0m,
            YieldRate: ToDomainDecimal(request.YieldRate) ?? 100m,
            MinLotSize: ToDomainDecimal(request.MinLotSize) ?? 1m,
            LotIncrement: ToDomainDecimal(request.LotIncrement) ?? 1m,
            MaxLotSize: ToDomainDecimal(request.MaxLotSize),
            ShelfLife: request.ShelfLife == 0 ? null : request.ShelfLife
        );
    }

    /// <summary>
    /// UpdateItemRequest を UpdateItemCommand に変換
    /// </summary>
    public UpdateItemCommand ToUpdateCommand(UpdateItemRequest request)
    {
        return new UpdateItemCommand(
            ItemCode: request.ItemCode,
            ItemName: EmptyToNull(request.ItemName),
            Category: request.Category == ProtoItemCategory.Unspecified
                ? null
                : ToDomainCategory(request.Category),
            UnitCode: EmptyToNull(request.UnitCode),
            LeadTime: request.LeadTime == 0 ? null : request.LeadTime,
            SafetyLeadTime: request.SafetyLeadTime == 0 ? null : request.SafetyLeadTime,
            SafetyStock: ToDomainDecimal(request.SafetyStock),
            YieldRate: ToDomainDecimal(request.YieldRate),
            MinLotSize: ToDomainDecimal(request.MinLotSize),
            LotIncrement: ToDomainDecimal(request.LotIncrement),
            MaxLotSize: ToDomainDecimal(request.MaxLotSize),
            ShelfLife: request.ShelfLife == 0 ? null : request.ShelfLife
        );
    }

    // ========== BOM ==========

    /// <summary>
    /// BomNode を Proto メッセージに変換
    /// </summary>
    public BomNodeMessage ToProto(BomNode domain)
    {
        var message = new BomNodeMessage
        {
            ItemCode = domain.ItemCode,
            ItemName = domain.ItemName,
            RequiredQuantity = ToProtoDecimal(domain.RequiredQuantity),
            Level = domain.Level
        };

        if (domain.Children != null)
        {
            message.Children.AddRange(domain.Children.Select(ToProto));
        }

        return message;
    }

    /// <summary>
    /// WhereUsedResult を Proto メッセージに変換
    /// </summary>
    public WhereUsedResultMessage ToProto(WhereUsedResult domain)
    {
        return new WhereUsedResultMessage
        {
            ParentItemCode = domain.ParentItemCode,
            ItemName = domain.ItemName,
            RequiredQuantity = ToProtoDecimal(domain.RequiredQuantity)
        };
    }

    // ========== Date ==========

    /// <summary>
    /// DateOnly を Proto Date に変換
    /// </summary>
    public Date ToProtoDate(DateOnly date)
    {
        return new Date
        {
            Year = date.Year,
            Month = date.Month,
            Day = date.Day
        };
    }

    /// <summary>
    /// Proto Date を DateOnly に変換
    /// </summary>
    public DateOnly ToDomainDate(Date protoDate)
    {
        if (protoDate == null || protoDate.Year == 0)
            return DateOnly.MinValue;

        return new DateOnly(protoDate.Year, protoDate.Month, protoDate.Day);
    }

    // ========== Decimal ==========

    /// <summary>
    /// decimal を Proto Decimal に変換
    /// </summary>
    public ProtoDecimal ToProtoDecimal(decimal value)
    {
        return new ProtoDecimal { Value = value.ToString() };
    }

    /// <summary>
    /// Proto Decimal を decimal? に変換
    /// </summary>
    public decimal? ToDomainDecimal(ProtoDecimal? protoDecimal)
    {
        if (protoDecimal == null || string.IsNullOrEmpty(protoDecimal.Value))
            return null;

        return decimal.Parse(protoDecimal.Value);
    }

    // ========== ItemCategory ==========

    /// <summary>
    /// ドメイン ItemCategory を Proto ItemCategory に変換
    /// </summary>
    public ProtoItemCategory ToProtoCategory(DomainItemCategory domain)
    {
        return domain switch
        {
            DomainItemCategory.Product => ProtoItemCategory.Product,
            DomainItemCategory.SemiProduct => ProtoItemCategory.SemiProduct,
            DomainItemCategory.Intermediate => ProtoItemCategory.Intermediate,
            DomainItemCategory.Part => ProtoItemCategory.Part,
            DomainItemCategory.Material => ProtoItemCategory.Material,
            DomainItemCategory.RawMaterial => ProtoItemCategory.RawMaterial,
            DomainItemCategory.Supply => ProtoItemCategory.Supply,
            _ => ProtoItemCategory.Unspecified
        };
    }

    /// <summary>
    /// Proto ItemCategory をドメイン ItemCategory に変換
    /// </summary>
    public DomainItemCategory ToDomainCategory(ProtoItemCategory proto)
    {
        return proto switch
        {
            ProtoItemCategory.Product => DomainItemCategory.Product,
            ProtoItemCategory.SemiProduct => DomainItemCategory.SemiProduct,
            ProtoItemCategory.Intermediate => DomainItemCategory.Intermediate,
            ProtoItemCategory.Part => DomainItemCategory.Part,
            ProtoItemCategory.Material => DomainItemCategory.Material,
            ProtoItemCategory.RawMaterial => DomainItemCategory.RawMaterial,
            ProtoItemCategory.Supply => DomainItemCategory.Supply,
            _ => DomainItemCategory.Product
        };
    }

    // ========== PurchaseOrder ==========

    /// <summary>
    /// PurchaseOrder を Proto メッセージに変換
    /// </summary>
    public PurchaseOrderMessage ToProto(PurchaseOrder domain)
    {
        var message = new PurchaseOrderMessage
        {
            PurchaseOrderNumber = domain.PurchaseOrderNumber,
            SupplierCode = domain.SupplierCode,
            OrderDate = ToProtoDate(domain.OrderDate),
            Status = ToProtoPurchaseOrderStatus(domain.Status),
            Remarks = domain.Remarks ?? ""
        };

        foreach (var detail in domain.Details)
        {
            message.Details.Add(ToProto(detail));
        }

        return message;
    }

    /// <summary>
    /// PurchaseOrderDetail を Proto メッセージに変換
    /// </summary>
    public PurchaseOrderDetailMessage ToProto(PurchaseOrderDetail domain)
    {
        return new PurchaseOrderDetailMessage
        {
            LineNumber = domain.LineNumber,
            ItemCode = domain.ItemCode,
            OrderQuantity = ToProtoDecimal(domain.OrderQuantity),
            OrderUnitPrice = ToProtoDecimal(domain.OrderUnitPrice),
            ExpectedReceivingDate = ToProtoDate(domain.ExpectedReceivingDate),
            ReceivedQuantity = ToProtoDecimal(domain.ReceivedQuantity),
            OrderAmount = ToProtoDecimal(domain.OrderAmount),
            CompletedFlag = domain.CompletedFlag
        };
    }

    /// <summary>
    /// CreatePurchaseOrderRequest を PurchaseOrderCreateCommand に変換
    /// </summary>
    public PurchaseOrderCreateCommand ToPurchaseOrderCreateCommand(CreatePurchaseOrderRequest request)
    {
        return new PurchaseOrderCreateCommand
        {
            SupplierCode = request.SupplierCode,
            OrderDate = DateOnly.FromDateTime(DateTime.Today),
            Remarks = EmptyToNull(request.Remarks),
            Details = request.Details.Select(d => new PurchaseOrderDetailCommand
            {
                ItemCode = d.ItemCode,
                OrderQuantity = ToDomainDecimal(d.OrderQuantity) ?? 0m,
                ExpectedReceivingDate = ToDomainDate(d.ExpectedReceivingDate)
            }).ToList()
        };
    }

    /// <summary>
    /// ドメイン PurchaseOrderStatus を Proto PurchaseOrderStatus に変換
    /// </summary>
    public ProtoPurchaseOrderStatus ToProtoPurchaseOrderStatus(DomainPurchaseOrderStatus domain)
    {
        return domain switch
        {
            DomainPurchaseOrderStatus.Creating => ProtoPurchaseOrderStatus.Creating,
            DomainPurchaseOrderStatus.Ordered => ProtoPurchaseOrderStatus.Ordered,
            DomainPurchaseOrderStatus.PartiallyReceived => ProtoPurchaseOrderStatus.PartiallyReceived,
            DomainPurchaseOrderStatus.Received => ProtoPurchaseOrderStatus.Received,
            DomainPurchaseOrderStatus.Accepted => ProtoPurchaseOrderStatus.Accepted,
            DomainPurchaseOrderStatus.Cancelled => ProtoPurchaseOrderStatus.Cancelled,
            _ => ProtoPurchaseOrderStatus.Unspecified
        };
    }

    /// <summary>
    /// Proto PurchaseOrderStatus をドメイン PurchaseOrderStatus に変換
    /// </summary>
    public DomainPurchaseOrderStatus ToDomainPurchaseOrderStatus(ProtoPurchaseOrderStatus proto)
    {
        return proto switch
        {
            ProtoPurchaseOrderStatus.Creating => DomainPurchaseOrderStatus.Creating,
            ProtoPurchaseOrderStatus.Ordered => DomainPurchaseOrderStatus.Ordered,
            ProtoPurchaseOrderStatus.PartiallyReceived => DomainPurchaseOrderStatus.PartiallyReceived,
            ProtoPurchaseOrderStatus.Received => DomainPurchaseOrderStatus.Received,
            ProtoPurchaseOrderStatus.Accepted => DomainPurchaseOrderStatus.Accepted,
            ProtoPurchaseOrderStatus.Cancelled => DomainPurchaseOrderStatus.Cancelled,
            _ => DomainPurchaseOrderStatus.Creating
        };
    }

    // ========== MRP ==========

    /// <summary>
    /// Requirement を Proto メッセージに変換
    /// </summary>
    public RequirementMessage ToProto(Requirement domain)
    {
        return new RequirementMessage
        {
            Id = domain.Id,
            RequirementNumber = domain.RequirementNumber,
            OrderId = domain.OrderId,
            ItemCode = domain.ItemCode,
            DueDate = ToProtoDate(domain.DueDate),
            RequiredQuantity = ToProtoDecimal(domain.RequiredQuantity),
            AllocatedQuantity = ToProtoDecimal(domain.AllocatedQuantity),
            ShortageQuantity = ToProtoDecimal(domain.ShortageQuantity),
            LocationCode = domain.LocationCode
        };
    }

    /// <summary>
    /// Allocation を Proto メッセージに変換
    /// </summary>
    public AllocationMessage ToProto(Allocation domain)
    {
        return new AllocationMessage
        {
            Id = domain.Id,
            RequirementId = domain.RequirementId,
            AllocationType = domain.AllocationType.ToString(),
            AllocationDate = ToProtoDate(domain.AllocationDate),
            AllocatedQuantity = ToProtoDecimal(domain.AllocatedQuantity),
            LocationCode = domain.LocationCode
        };
    }

    /// <summary>
    /// Order を PlannedOrderMessage に変換
    /// </summary>
    public PlannedOrderMessage ToProto(Order domain)
    {
        return new PlannedOrderMessage
        {
            Id = domain.Id,
            OrderNumber = domain.OrderNumber,
            OrderType = ToProtoOrderType(domain.OrderType),
            ItemCode = domain.ItemCode,
            StartDate = ToProtoDate(domain.StartDate),
            DueDate = ToProtoDate(domain.DueDate),
            PlanQuantity = ToProtoDecimal(domain.PlanQuantity),
            LocationCode = domain.LocationCode,
            Status = ToProtoPlanStatus(domain.Status)
        };
    }

    /// <summary>
    /// ドメイン OrderType を Proto OrderType に変換
    /// </summary>
    public ProtoOrderType ToProtoOrderType(DomainOrderType domain)
    {
        return domain switch
        {
            DomainOrderType.Purchase => ProtoOrderType.Purchase,
            DomainOrderType.Manufacturing => ProtoOrderType.Manufacture,
            _ => ProtoOrderType.Unspecified
        };
    }

    /// <summary>
    /// Proto OrderType をドメイン OrderType に変換
    /// </summary>
    public DomainOrderType ToDomainOrderType(ProtoOrderType proto)
    {
        return proto switch
        {
            ProtoOrderType.Purchase => DomainOrderType.Purchase,
            ProtoOrderType.Manufacture => DomainOrderType.Manufacturing,
            _ => DomainOrderType.Manufacturing
        };
    }

    /// <summary>
    /// ドメイン PlanStatus を Proto PlanStatus に変換
    /// </summary>
    public ProtoPlanStatus ToProtoPlanStatus(DomainPlanStatus domain)
    {
        return domain switch
        {
            DomainPlanStatus.Draft => ProtoPlanStatus.Draft,
            DomainPlanStatus.Confirmed => ProtoPlanStatus.Confirmed,
            DomainPlanStatus.Expanded => ProtoPlanStatus.InProgress,
            DomainPlanStatus.Cancelled => ProtoPlanStatus.Cancelled,
            _ => ProtoPlanStatus.Unspecified
        };
    }

    // ========== Utility ==========

    private static string? EmptyToNull(string? value)
        => string.IsNullOrEmpty(value) ? null : value;
}
