package com.example.production.domain.model.subcontract;

import lombok.*;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 支給データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Supply {
    private Integer id;
    private String supplyNumber;
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private String supplierCode;
    private LocalDate supplyDate;
    private String supplierPersonCode;
    private SupplyType supplyType;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;
}
