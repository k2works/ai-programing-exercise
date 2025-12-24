package com.example.production.domain.model.plan;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class MasterProductionSchedule {
    private Integer id;
    private String mpsNumber;
    private LocalDate planDate;
    private String itemCode;
    private BigDecimal planQuantity;
    private LocalDate dueDate;
    @Builder.Default
    private PlanStatus status = PlanStatus.DRAFT;
    private String locationCode;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
