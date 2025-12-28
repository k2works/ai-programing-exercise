package com.example.production.infrastructure.in.graphql.resolver;

import com.example.production.application.port.out.OrderRepository;
import com.example.production.application.port.out.RequirementRepository;
import com.example.production.application.service.MrpService;
import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.Requirement;
import lombok.Builder;
import lombok.Data;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.MutationMapping;
import org.springframework.stereotype.Controller;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.UUID;

/**
 * MRP GraphQL リゾルバ実装
 * 既存の MrpService を Input Adapter として呼び出す
 */
@Controller
public class MrpResolver {

    private final MrpService mrpService;
    private final OrderRepository orderRepository;
    private final RequirementRepository requirementRepository;

    public MrpResolver(MrpService mrpService,
                       OrderRepository orderRepository,
                       RequirementRepository requirementRepository) {
        this.mrpService = mrpService;
        this.orderRepository = orderRepository;
        this.requirementRepository = requirementRepository;
    }

    /**
     * MRP 実行
     */
    @MutationMapping
    public MrpResult executeMrp(@Argument ExecuteMrpInput input) {
        String executionId = UUID.randomUUID().toString();
        long startTime = System.currentTimeMillis();

        // 期間内のオーダと所要量を取得（期間フィルタリング）
        List<Order> orders = orderRepository.findAll().stream()
            .filter(order -> order.getDueDate() != null)
            .filter(order -> !order.getDueDate().isBefore(input.getStartDate()))
            .filter(order -> !order.getDueDate().isAfter(input.getEndDate()))
            .toList();

        List<Requirement> requirements = requirementRepository.findAll().stream()
            .filter(req -> req.getDueDate() != null)
            .filter(req -> !req.getDueDate().isBefore(input.getStartDate()))
            .filter(req -> !req.getDueDate().isAfter(input.getEndDate()))
            .toList();

        long executionTime = System.currentTimeMillis() - startTime;

        // オーダを計画オーダに変換
        List<PlannedOrder> plannedOrders = orders.stream()
            .map(order -> PlannedOrder.builder()
                .itemCode(order.getItemCode())
                .itemName(order.getItemCode()) // 簡略化（実際は品目名を取得）
                .quantity(order.getPlanQuantity())
                .dueDate(order.getDueDate())
                .orderType(order.getOrderType().name())
                .build())
            .toList();

        // 不足品目を抽出
        List<ShortageItem> shortageItems = requirements.stream()
            .filter(req -> req.getShortageQuantity().compareTo(BigDecimal.ZERO) > 0)
            .map(req -> ShortageItem.builder()
                .itemCode(req.getItemCode())
                .itemName(req.getItemCode()) // 簡略化（実際は品目名を取得）
                .shortageQuantity(req.getShortageQuantity())
                .recommendedOrderDate(req.getDueDate().minusDays(7))
                .build())
            .toList();

        // 統計情報を集計
        long purchaseOrderCount = orders.stream()
            .filter(o -> o.getOrderType() == OrderType.PURCHASE)
            .count();
        long productionOrderCount = orders.stream()
            .filter(o -> o.getOrderType() == OrderType.MANUFACTURING)
            .count();

        return MrpResult.builder()
            .executionId(executionId)
            .periodStart(input.getStartDate())
            .periodEnd(input.getEndDate())
            .plannedOrders(plannedOrders)
            .shortageItems(shortageItems)
            .statistics(MrpStatistics.builder()
                .totalItemsProcessed(orders.size())
                .purchaseOrderCount((int) purchaseOrderCount)
                .productionOrderCount((int) productionOrderCount)
                .shortageItemCount(shortageItems.size())
                .executionTimeMs(executionTime)
                .build())
            .build();
    }
}

/**
 * MRP 実行入力
 */
@Data
class ExecuteMrpInput {
    private LocalDate startDate;
    private LocalDate endDate;
}

/**
 * MRP 実行結果
 */
@Data
@Builder
class MrpResult {
    private String executionId;
    private LocalDate periodStart;
    private LocalDate periodEnd;
    private List<PlannedOrder> plannedOrders;
    private List<ShortageItem> shortageItems;
    private MrpStatistics statistics;
}

/**
 * 計画オーダ
 */
@Data
@Builder
class PlannedOrder {
    private String itemCode;
    private String itemName;
    private BigDecimal quantity;
    private LocalDate dueDate;
    private String orderType;
}

/**
 * 不足品目
 */
@Data
@Builder
class ShortageItem {
    private String itemCode;
    private String itemName;
    private BigDecimal shortageQuantity;
    private LocalDate recommendedOrderDate;
}

/**
 * MRP 統計情報
 */
@Data
@Builder
class MrpStatistics {
    private int totalItemsProcessed;
    private int purchaseOrderCount;
    private int productionOrderCount;
    private int shortageItemCount;
    private long executionTimeMs;
}
