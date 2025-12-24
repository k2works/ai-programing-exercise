package com.example.production.domain.model.plan;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Order {
    private Integer id;
    private String orderNumber;
    private OrderType orderType;
    private String itemCode;
    private LocalDate startDate;
    private LocalDate dueDate;
    private LocalDate expirationDate;
    private BigDecimal planQuantity;
    private String locationCode;
    @Builder.Default
    private PlanStatus status = PlanStatus.DRAFT;
    private Integer mpsId;
    private Integer parentOrderId;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
