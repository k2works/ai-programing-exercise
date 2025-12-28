package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;

/**
 * 発注レスポンス
 */
@Value
@Builder
public class PurchaseOrderResponse {
    String purchaseOrderNumber;
    LocalDate orderDate;
    String supplierCode;
    String ordererCode;
    String departmentCode;
    PurchaseOrderStatus status;
    String remarks;
    List<PurchaseOrderDetailResponse> details;

    public static PurchaseOrderResponse from(PurchaseOrder order) {
        return PurchaseOrderResponse.builder()
                .purchaseOrderNumber(order.getPurchaseOrderNumber())
                .orderDate(order.getOrderDate())
                .supplierCode(order.getSupplierCode())
                .ordererCode(order.getOrdererCode())
                .departmentCode(order.getDepartmentCode())
                .status(order.getStatus())
                .remarks(order.getRemarks())
                .details(order.getDetails() != null
                        ? order.getDetails().stream()
                            .map(PurchaseOrderDetailResponse::from)
                            .toList()
                        : Collections.emptyList())
                .build();
    }

    @Value
    @Builder
    public static class PurchaseOrderDetailResponse {
        Integer lineNumber;
        String itemCode;
        BigDecimal orderQuantity;
        BigDecimal orderUnitPrice;
        LocalDate expectedReceivingDate;

        public static PurchaseOrderDetailResponse from(PurchaseOrderDetail detail) {
            return PurchaseOrderDetailResponse.builder()
                    .lineNumber(detail.getLineNumber())
                    .itemCode(detail.getItemCode())
                    .orderQuantity(detail.getOrderQuantity())
                    .orderUnitPrice(detail.getOrderUnitPrice())
                    .expectedReceivingDate(detail.getExpectedReceivingDate())
                    .build();
        }
    }
}
