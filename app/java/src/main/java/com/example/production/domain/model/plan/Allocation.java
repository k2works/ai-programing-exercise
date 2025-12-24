package com.example.production.domain.model.plan;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Allocation {
    private Integer id;
    private Integer requirementId;
    private AllocationType allocationType;
    private Integer orderId;
    private LocalDate allocationDate;
    private BigDecimal allocatedQuantity;
    private String locationCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
