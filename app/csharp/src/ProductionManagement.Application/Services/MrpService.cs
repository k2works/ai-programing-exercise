using ProductionManagement.Application.Port.Out;
using ProductionManagement.Domain.Models.Plan;

namespace ProductionManagement.Application.Services;

/// <summary>
/// 所要量展開（MRP）サービス
/// </summary>
public class MrpService
{
    private readonly IItemRepository _itemRepository;
    private readonly IBomRepository _bomRepository;
    private readonly IOrderRepository _orderRepository;
    private readonly IRequirementRepository _requirementRepository;
    private readonly IAllocationRepository _allocationRepository;

    public MrpService(
        IItemRepository itemRepository,
        IBomRepository bomRepository,
        IOrderRepository orderRepository,
        IRequirementRepository requirementRepository,
        IAllocationRepository allocationRepository)
    {
        _itemRepository = itemRepository;
        _bomRepository = bomRepository;
        _orderRepository = orderRepository;
        _requirementRepository = requirementRepository;
        _allocationRepository = allocationRepository;
    }

    /// <summary>
    /// オーダから所要量を展開する
    /// </summary>
    public async Task<IReadOnlyList<Requirement>> ExplodeRequirementsAsync(int orderId)
    {
        var order = await _orderRepository.FindByIdAsync(orderId)
            ?? throw new ArgumentException($"Order not found: {orderId}");

        var bomList = await _bomRepository.FindByParentItemCodeAndDateAsync(
            order.ItemCode,
            order.DueDate
        );

        var requirements = new List<Requirement>();
        var lineNumber = 0;

        foreach (var bom in bomList)
        {
            lineNumber++;
            var childItem = await _itemRepository.FindByItemCodeAndDateAsync(
                bom.ChildItemCode,
                order.DueDate
            );

            if (childItem == null) continue;

            // 所要量 = 親オーダ数量 × (必要数量 / 基準数量) × (1 + 不良率)
            var requiredQuantity = order.PlanQuantity
                * (bom.RequiredQuantity / bom.BaseQuantity)
                * (1 + bom.DefectRate / 100m);

            // 子品目のリードタイムを考慮した納期
            int leadTime = childItem.LeadTime;
            int safetyLeadTime = childItem.SafetyLeadTime;
            var childDueDate = CalculateStartDate(order.StartDate, leadTime, safetyLeadTime);

            // 所要NOは20文字以内: REQ-{orderId:5桁}-{行番号:3桁}
            var requirement = new Requirement
            {
                RequirementNumber = $"REQ-{order.Id:D5}-{lineNumber:D3}",
                OrderId = order.Id,
                ItemCode = bom.ChildItemCode,
                DueDate = childDueDate,
                RequiredQuantity = Math.Round(requiredQuantity, 2),
                AllocatedQuantity = 0m,
                ShortageQuantity = Math.Round(requiredQuantity, 2),
                LocationCode = order.LocationCode
            };
            await _requirementRepository.SaveAsync(requirement);

            requirements.Add(requirement);
        }

        return requirements;
    }

    /// <summary>
    /// 在庫から引当を行う
    /// </summary>
    public async Task<Allocation> AllocateFromInventoryAsync(int requirementId, decimal inventoryQuantity)
    {
        var requirement = await _requirementRepository.FindByIdAsync(requirementId)
            ?? throw new ArgumentException($"Requirement not found: {requirementId}");

        var allocatedQuantity = Math.Min(requirement.ShortageQuantity, inventoryQuantity);
        var newAllocatedQuantity = requirement.AllocatedQuantity + allocatedQuantity;
        var shortageQuantity = requirement.RequiredQuantity - newAllocatedQuantity;

        var allocation = new Allocation
        {
            RequirementId = requirementId,
            AllocationType = AllocationType.Inventory,
            AllocationDate = DateOnly.FromDateTime(DateTime.Today),
            AllocatedQuantity = allocatedQuantity,
            LocationCode = requirement.LocationCode
        };
        await _allocationRepository.SaveAsync(allocation);

        await _requirementRepository.UpdateAllocationAsync(requirementId, newAllocatedQuantity, shortageQuantity);

        return allocation;
    }

    /// <summary>
    /// ロットサイズを考慮したオーダ数量を計算する
    /// </summary>
    public decimal CalculateOrderQuantity(
        decimal requiredQuantity,
        decimal? minimumLotSize,
        decimal? incrementLotSize,
        decimal? maximumLotSize)
    {
        minimumLotSize ??= 1m;
        incrementLotSize ??= 1m;

        // 最小ロットに満たない場合
        if (requiredQuantity <= minimumLotSize)
        {
            if (maximumLotSize.HasValue && minimumLotSize > maximumLotSize)
            {
                return maximumLotSize.Value;
            }
            return minimumLotSize.Value;
        }

        // 刻みロットで切り上げ
        var difference = requiredQuantity - minimumLotSize.Value;
        var lots = Math.Ceiling(difference / incrementLotSize.Value);
        var orderQuantity = minimumLotSize.Value + lots * incrementLotSize.Value;

        // 最大ロットを超える場合は制限
        if (maximumLotSize.HasValue && orderQuantity > maximumLotSize.Value)
        {
            return maximumLotSize.Value;
        }

        return orderQuantity;
    }

    /// <summary>
    /// リードタイムから着手日を計算する
    /// </summary>
    public DateOnly CalculateStartDate(DateOnly dueDate, int leadTime, int safetyLeadTime)
    {
        return dueDate.AddDays(-(leadTime + safetyLeadTime));
    }

    /// <summary>
    /// 不足分に対して新規オーダを生成する
    /// </summary>
    public async Task<Order> CreateShortageOrderAsync(
        string itemCode,
        decimal shortageQuantity,
        DateOnly dueDate,
        string locationCode,
        OrderType orderType)
    {
        var item = await _itemRepository.FindByItemCodeAndDateAsync(itemCode, DateOnly.FromDateTime(DateTime.Today))
            ?? throw new ArgumentException($"Item not found: {itemCode}");

        var orderQuantity = CalculateOrderQuantity(
            shortageQuantity,
            item.MinLotSize,
            item.LotIncrement,
            item.MaxLotSize
        );

        int leadTime = item.LeadTime;
        int safetyLeadTime = item.SafetyLeadTime;
        var startDate = CalculateStartDate(dueDate, leadTime, safetyLeadTime);

        var order = new Order
        {
            OrderNumber = $"{(orderType == OrderType.Purchase ? "PO" : "MO")}-{DateTimeOffset.UtcNow.ToUnixTimeMilliseconds()}",
            OrderType = orderType,
            ItemCode = itemCode,
            StartDate = startDate,
            DueDate = dueDate,
            PlanQuantity = orderQuantity,
            LocationCode = locationCode,
            Status = PlanStatus.Draft
        };
        await _orderRepository.SaveAsync(order);

        return order;
    }

    /// <summary>
    /// 歩留率と不良率を考慮した所要量計算
    /// </summary>
    public decimal CalculateRequiredQuantity(
        decimal parentQuantity,
        decimal baseQuantity,
        decimal requiredQuantity,
        decimal defectRate,
        decimal yieldRate)
    {
        // 基本所要量
        var basicQuantity = parentQuantity * requiredQuantity / baseQuantity;

        // 不良率を考慮
        var afterDefect = basicQuantity * (1 + defectRate / 100m);

        // 歩留率を考慮
        var afterYield = Math.Ceiling(afterDefect / (yieldRate / 100m));

        return afterYield;
    }
}
