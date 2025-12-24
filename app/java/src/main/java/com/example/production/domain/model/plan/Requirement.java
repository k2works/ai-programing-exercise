package com.example.production.domain.model.plan;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Requirement {
    private Integer id;
    private String requirementNumber;
    private Integer orderId;
    private String itemCode;
    private LocalDate dueDate;
    private BigDecimal requiredQuantity;
    @Builder.Default
    private BigDecimal allocatedQuantity = BigDecimal.ZERO;
    @Builder.Default
    private BigDecimal shortageQuantity = BigDecimal.ZERO;
    private String locationCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
