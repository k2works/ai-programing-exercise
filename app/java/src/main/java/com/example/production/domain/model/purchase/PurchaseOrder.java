package com.example.production.domain.model.purchase;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Comparator;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PurchaseOrder {
    private Integer id;
    private String purchaseOrderNumber;
    private LocalDate orderDate;
    private String supplierCode;
    private String ordererCode;
    private String departmentCode;
    @Builder.Default
    private PurchaseOrderStatus status = PurchaseOrderStatus.CREATING;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // リレーション
    private List<PurchaseOrderDetail> details;

    /**
     * 発注合計金額を取得する
     */
    public BigDecimal getTotalOrderAmount() {
        if (details == null || details.isEmpty()) {
            return BigDecimal.ZERO;
        }
        return details.stream()
                .map(d -> d.getOrderAmount() != null ? d.getOrderAmount() : BigDecimal.ZERO)
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * 最も早い入荷予定日を取得する
     */
    public LocalDate getEarliestDeliveryDate() {
        if (details == null || details.isEmpty()) {
            return null;
        }
        return details.stream()
                .map(PurchaseOrderDetail::getExpectedReceivingDate)
                .filter(d -> d != null)
                .min(Comparator.naturalOrder())
                .orElse(null);
    }
}
