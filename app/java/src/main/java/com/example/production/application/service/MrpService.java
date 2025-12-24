package com.example.production.application.service;

import com.example.production.application.port.out.*;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.plan.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

/**
 * 所要量展開（MRP）サービス
 */
@Service
@RequiredArgsConstructor
public class MrpService {

    private final ItemRepository itemRepository;
    private final BomRepository bomRepository;
    private final OrderRepository orderRepository;
    private final RequirementRepository requirementRepository;
    private final AllocationRepository allocationRepository;

    /**
     * オーダから所要量を展開する
     */
    @Transactional
    public List<Requirement> explodeRequirements(Integer orderId) {
        var order = orderRepository.findById(orderId)
                .orElseThrow(() -> new IllegalArgumentException("Order not found: " + orderId));

        var bomList = bomRepository.findByParentItemCodeAndDate(
                order.getItemCode(),
                order.getDueDate()
        );

        List<Requirement> requirements = new ArrayList<>();

        for (var bom : bomList) {
            var childItem = itemRepository.findByItemCodeAndDate(
                    bom.getChildItemCode(),
                    order.getDueDate()
            );

            if (childItem.isEmpty()) continue;

            // 所要量 = 親オーダ数量 × (必要数量 / 基準数量) × (1 + 不良率)
            var requiredQuantity = order.getPlanQuantity()
                    .multiply(bom.getRequiredQuantity())
                    .divide(bom.getBaseQuantity(), 2, RoundingMode.HALF_UP)
                    .multiply(BigDecimal.ONE.add(
                            bom.getDefectRate().divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP)
                    ));

            // 子品目のリードタイムを考慮した納期
            var item = childItem.get();
            int leadTime = item.getLeadTime() != null ? item.getLeadTime() : 0;
            int safetyLeadTime = item.getSafetyLeadTime() != null ? item.getSafetyLeadTime() : 0;
            var childDueDate = calculateStartDate(order.getStartDate(), leadTime, safetyLeadTime);

            var requirement = Requirement.builder()
                    .requirementNumber(generateRequirementNumber())
                    .orderId(order.getId())
                    .itemCode(bom.getChildItemCode())
                    .dueDate(childDueDate)
                    .requiredQuantity(requiredQuantity)
                    .allocatedQuantity(BigDecimal.ZERO)
                    .shortageQuantity(requiredQuantity)
                    .locationCode(order.getLocationCode())
                    .build();
            requirementRepository.save(requirement);

            requirements.add(requirement);
        }

        return requirements;
    }

    /**
     * 在庫から引当を行う
     */
    @Transactional
    public Allocation allocateFromInventory(Integer requirementId, int inventoryQuantity) {
        var requirement = requirementRepository.findById(requirementId)
                .orElseThrow(() -> new IllegalArgumentException("Requirement not found: " + requirementId));

        var requiredQuantity = requirement.getRequiredQuantity();
        var allocatedQuantity = requiredQuantity.min(new BigDecimal(inventoryQuantity));
        var shortageQuantity = requiredQuantity.subtract(allocatedQuantity);

        var allocation = Allocation.builder()
                .requirementId(requirementId)
                .allocationType(AllocationType.INVENTORY)
                .allocationDate(LocalDate.now())
                .allocatedQuantity(allocatedQuantity)
                .locationCode(requirement.getLocationCode())
                .build();
        allocationRepository.save(allocation);

        requirementRepository.updateAllocation(requirementId, allocatedQuantity, shortageQuantity);

        return allocation;
    }

    /**
     * ロットサイズを考慮したオーダ数量を計算する
     */
    public BigDecimal calculateOrderQuantity(
            BigDecimal requiredQuantity,
            BigDecimal minimumLotSize,
            BigDecimal incrementLotSize,
            BigDecimal maximumLotSize
    ) {
        if (minimumLotSize == null) minimumLotSize = BigDecimal.ONE;
        if (incrementLotSize == null) incrementLotSize = BigDecimal.ONE;

        // 最小ロットに満たない場合
        if (requiredQuantity.compareTo(minimumLotSize) <= 0) {
            if (maximumLotSize != null && minimumLotSize.compareTo(maximumLotSize) > 0) {
                return maximumLotSize;
            }
            return minimumLotSize;
        }

        // 刻みロットで切り上げ
        BigDecimal difference = requiredQuantity.subtract(minimumLotSize);
        BigDecimal lots = difference.divide(incrementLotSize, 0, RoundingMode.CEILING);
        BigDecimal orderQuantity = minimumLotSize.add(lots.multiply(incrementLotSize));

        // 最大ロットを超える場合は制限
        if (maximumLotSize != null && orderQuantity.compareTo(maximumLotSize) > 0) {
            return maximumLotSize;
        }

        return orderQuantity;
    }

    /**
     * リードタイムから着手日を計算する
     */
    public LocalDate calculateStartDate(LocalDate dueDate, int leadTime, int safetyLeadTime) {
        return dueDate.minusDays(leadTime + safetyLeadTime);
    }

    /**
     * 不足分に対して新規オーダを生成する
     */
    @Transactional
    public Order createShortageOrder(
            String itemCode,
            BigDecimal shortageQuantity,
            LocalDate dueDate,
            String locationCode,
            OrderType orderType
    ) {
        var item = itemRepository.findByItemCodeAndDate(itemCode, LocalDate.now())
                .orElseThrow(() -> new IllegalArgumentException("Item not found: " + itemCode));

        var orderQuantity = calculateOrderQuantity(
                shortageQuantity,
                item.getMinLotSize(),
                item.getLotIncrement(),
                item.getMaxLotSize()
        );

        int leadTime = item.getLeadTime() != null ? item.getLeadTime() : 0;
        int safetyLeadTime = item.getSafetyLeadTime() != null ? item.getSafetyLeadTime() : 0;
        var startDate = calculateStartDate(dueDate, leadTime, safetyLeadTime);

        var order = Order.builder()
                .orderNumber((orderType == OrderType.PURCHASE ? "PO" : "MO") + "-" + System.currentTimeMillis())
                .orderType(orderType)
                .itemCode(itemCode)
                .startDate(startDate)
                .dueDate(dueDate)
                .planQuantity(orderQuantity)
                .locationCode(locationCode)
                .status(PlanStatus.DRAFT)
                .build();
        orderRepository.save(order);

        return order;
    }

    /**
     * MRP の完全実行
     */
    @Transactional
    public void executeMrp(Integer mpsId, MpsRepository mpsRepository) {
        var mps = mpsRepository.findById(mpsId)
                .orElseThrow(() -> new IllegalArgumentException("MPS not found: " + mpsId));

        var orders = orderRepository.findByMpsId(mpsId);

        Order productOrder;
        if (orders.isEmpty()) {
            productOrder = Order.builder()
                    .orderNumber("MO-" + System.currentTimeMillis())
                    .orderType(OrderType.MANUFACTURING)
                    .itemCode(mps.getItemCode())
                    .startDate(calculateStartDate(mps.getDueDate(), 5, 0))
                    .dueDate(mps.getDueDate())
                    .planQuantity(mps.getPlanQuantity())
                    .locationCode(mps.getLocationCode() != null ? mps.getLocationCode() : "WH-001")
                    .status(PlanStatus.CONFIRMED)
                    .mpsId(mpsId)
                    .build();
            orderRepository.save(productOrder);
        } else {
            productOrder = orders.get(0);
        }

        recursiveMrpExplosion(productOrder.getId());
    }

    /**
     * 再帰的な所要量展開
     */
    private void recursiveMrpExplosion(Integer orderId) {
        var requirements = explodeRequirements(orderId);

        for (var requirement : requirements) {
            var itemOpt = itemRepository.findByItemCodeAndDate(requirement.getItemCode(), LocalDate.now());
            if (itemOpt.isEmpty()) continue;
            var item = itemOpt.get();

            var orderType = (item.getItemCategory() == ItemCategory.MATERIAL ||
                    item.getItemCategory() == ItemCategory.RAW_MATERIAL)
                    ? OrderType.PURCHASE : OrderType.MANUFACTURING;

            var newOrder = createShortageOrder(
                    requirement.getItemCode(),
                    requirement.getShortageQuantity(),
                    requirement.getDueDate(),
                    requirement.getLocationCode(),
                    orderType
            );

            orderRepository.updateParentOrderId(newOrder.getId(), orderId);

            recursiveMrpExplosion(newOrder.getId());
        }
    }

    /**
     * 所要番号を生成する（REQ-XXXXXX形式、最大20文字）
     */
    private String generateRequirementNumber() {
        return "REQ-" + String.format("%06d", System.nanoTime() % 1000000);
    }

    /**
     * 歩留率と不良率を考慮した所要量計算
     */
    public BigDecimal calculateRequiredQuantity(
            BigDecimal parentQuantity,
            BigDecimal baseQuantity,
            BigDecimal requiredQuantity,
            BigDecimal defectRate,
            BigDecimal yieldRate
    ) {
        // 基本所要量
        BigDecimal basicQuantity = parentQuantity.multiply(requiredQuantity)
                .divide(baseQuantity, 4, RoundingMode.HALF_UP);

        // 不良率を考慮
        BigDecimal afterDefect = basicQuantity.multiply(
                BigDecimal.ONE.add(defectRate.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP))
        );

        // 歩留率を考慮
        BigDecimal afterYield = afterDefect.divide(
                yieldRate.divide(new BigDecimal("100"), 4, RoundingMode.HALF_UP),
                0,
                RoundingMode.CEILING
        );

        return afterYield;
    }
}
