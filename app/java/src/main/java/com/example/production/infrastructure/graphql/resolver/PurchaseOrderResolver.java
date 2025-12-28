package com.example.production.infrastructure.graphql.resolver;

import com.example.production.application.port.in.PurchaseOrderUseCase;
import com.example.production.application.port.in.command.CreatePurchaseOrderCommand;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import org.springframework.graphql.data.method.annotation.Argument;
import org.springframework.graphql.data.method.annotation.MutationMapping;
import org.springframework.graphql.data.method.annotation.QueryMapping;
import org.springframework.stereotype.Controller;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 発注 GraphQL リゾルバ実装
 * 既存の PurchaseOrderUseCase を Input Adapter として呼び出す
 */
@Controller
public class PurchaseOrderResolver {

    private final PurchaseOrderUseCase purchaseOrderUseCase;

    public PurchaseOrderResolver(PurchaseOrderUseCase purchaseOrderUseCase) {
        this.purchaseOrderUseCase = purchaseOrderUseCase;
    }

    // ========== Query ==========

    /**
     * 発注取得
     */
    @QueryMapping
    public PurchaseOrder purchaseOrder(@Argument String purchaseOrderNumber) {
        return purchaseOrderUseCase.getOrder(purchaseOrderNumber);
    }

    /**
     * 発注一覧（ページネーション付き）
     */
    @QueryMapping
    public PurchaseOrderConnection purchaseOrders(
            @Argument PurchaseOrderStatus status,
            @Argument Integer page,
            @Argument Integer size) {

        int pageNum = page != null ? page : 0;
        int pageSize = size != null ? size : 20;

        List<PurchaseOrder> orders = purchaseOrderUseCase.getAllOrders();

        // ステータスでフィルタリング
        if (status != null) {
            orders = orders.stream()
                .filter(order -> order.getStatus() == status)
                .toList();
        }

        int totalElements = orders.size();

        // ページング処理
        int start = pageNum * pageSize;
        int end = Math.min(start + pageSize, orders.size());
        List<PurchaseOrder> pagedOrders = start < orders.size()
            ? orders.subList(start, end)
            : List.of();

        return PurchaseOrderConnection.builder()
            .edges(pagedOrders.stream()
                .map(order -> PurchaseOrderEdge.builder()
                    .node(order)
                    .cursor(order.getPurchaseOrderNumber())
                    .build())
                .toList())
            .pageInfo(PageInfo.builder()
                .hasNextPage(end < totalElements)
                .hasPreviousPage(pageNum > 0)
                .totalElements(totalElements)
                .totalPages((int) Math.ceil((double) totalElements / pageSize))
                .build())
            .build();
    }

    // ========== Mutation ==========

    /**
     * 発注登録
     */
    @MutationMapping
    public PurchaseOrder createPurchaseOrder(@Argument CreatePurchaseOrderInput input) {
        CreatePurchaseOrderCommand command = CreatePurchaseOrderCommand.builder()
            .supplierCode(input.getSupplierCode())
            .ordererCode(input.getOrdererCode())
            .departmentCode(input.getDepartmentCode())
            .remarks(input.getRemarks())
            .details(input.getDetails().stream()
                .map(d -> CreatePurchaseOrderCommand.PurchaseOrderDetailCommand.builder()
                    .itemCode(d.getItemCode())
                    .orderQuantity(d.getOrderQuantity())
                    .unitPrice(d.getUnitPrice())
                    .deliveryDate(d.getDeliveryDate())
                    .build())
                .toList())
            .build();

        return purchaseOrderUseCase.createOrder(command);
    }

    /**
     * 発注確定
     */
    @MutationMapping
    public PurchaseOrder confirmPurchaseOrder(@Argument String purchaseOrderNumber) {
        return purchaseOrderUseCase.confirmOrder(purchaseOrderNumber);
    }

    /**
     * 発注取消
     */
    @MutationMapping
    public boolean cancelPurchaseOrder(@Argument String purchaseOrderNumber) {
        purchaseOrderUseCase.cancelOrder(purchaseOrderNumber);
        return true;
    }

    /**
     * 入荷処理
     */
    @MutationMapping
    public PurchaseOrder receivePurchaseOrder(@Argument String purchaseOrderNumber) {
        return purchaseOrderUseCase.receiveOrder(purchaseOrderNumber);
    }

    /**
     * 検収処理
     */
    @MutationMapping
    public PurchaseOrder acceptPurchaseOrder(@Argument String purchaseOrderNumber) {
        return purchaseOrderUseCase.acceptOrder(purchaseOrderNumber);
    }
}

/**
 * 発注一覧（ページネーション）
 */
@lombok.Data
@lombok.Builder
class PurchaseOrderConnection {
    private List<PurchaseOrderEdge> edges;
    private PageInfo pageInfo;
}

/**
 * 発注エッジ
 */
@lombok.Data
@lombok.Builder
class PurchaseOrderEdge {
    private PurchaseOrder node;
    private String cursor;
}

/**
 * 発注作成入力
 */
@lombok.Data
class CreatePurchaseOrderInput {
    private String supplierCode;
    private String ordererCode;
    private String departmentCode;
    private String remarks;
    private List<CreateOrderDetailInput> details;
}

/**
 * 発注明細作成入力
 */
@lombok.Data
class CreateOrderDetailInput {
    private String itemCode;
    private BigDecimal orderQuantity;
    private BigDecimal unitPrice;
    private LocalDate deliveryDate;
}
