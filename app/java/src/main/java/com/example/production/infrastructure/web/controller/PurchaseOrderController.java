package com.example.production.infrastructure.web.controller;

import com.example.production.application.port.in.PurchaseOrderUseCase;
import com.example.production.application.port.in.command.CreatePurchaseOrderCommand;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.infrastructure.web.dto.CreatePurchaseOrderRequest;
import com.example.production.infrastructure.web.dto.PurchaseOrderResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 発注 Controller（Input Adapter）
 */
@RestController
@RequestMapping("/api/purchase-orders")
@Tag(name = "purchase-orders", description = "発注 API")
@RequiredArgsConstructor
public class PurchaseOrderController {

    private final PurchaseOrderUseCase useCase;

    @GetMapping
    @Operation(summary = "発注一覧の取得")
    public ResponseEntity<List<PurchaseOrderResponse>> getAllOrders() {
        List<PurchaseOrder> orders = useCase.getAllOrders();
        return ResponseEntity.ok(orders.stream()
                .map(PurchaseOrderResponse::from)
                .toList());
    }

    @GetMapping("/{orderNumber}")
    @Operation(summary = "発注詳細の取得")
    public ResponseEntity<PurchaseOrderResponse> getOrder(
            @PathVariable String orderNumber) {
        PurchaseOrder order = useCase.getOrder(orderNumber);
        return ResponseEntity.ok(PurchaseOrderResponse.from(order));
    }

    @PostMapping
    @Operation(summary = "発注の登録")
    public ResponseEntity<PurchaseOrderResponse> createOrder(
            @Valid @RequestBody CreatePurchaseOrderRequest request) {

        CreatePurchaseOrderCommand command = CreatePurchaseOrderCommand.builder()
                .supplierCode(request.getSupplierCode())
                .ordererCode(request.getOrdererCode())
                .departmentCode(request.getDepartmentCode())
                .remarks(request.getRemarks())
                .details(request.getDetails().stream()
                        .map(d -> CreatePurchaseOrderCommand.PurchaseOrderDetailCommand.builder()
                                .itemCode(d.getItemCode())
                                .orderQuantity(d.getOrderQuantity())
                                .unitPrice(d.getUnitPrice())
                                .deliveryDate(d.getDeliveryDate())
                                .build())
                        .toList())
                .build();

        PurchaseOrder order = useCase.createOrder(command);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(PurchaseOrderResponse.from(order));
    }

    @PostMapping("/{orderNumber}/confirm")
    @Operation(summary = "発注の確定")
    public ResponseEntity<PurchaseOrderResponse> confirmOrder(
            @PathVariable String orderNumber) {
        PurchaseOrder order = useCase.confirmOrder(orderNumber);
        return ResponseEntity.ok(PurchaseOrderResponse.from(order));
    }

    @DeleteMapping("/{orderNumber}")
    @Operation(summary = "発注の取消")
    @ResponseStatus(HttpStatus.NO_CONTENT)
    public void cancelOrder(@PathVariable String orderNumber) {
        useCase.cancelOrder(orderNumber);
    }
}
