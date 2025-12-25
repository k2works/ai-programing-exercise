package com.example.production.application.service;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 外注発注入力DTO
 */
@Data
@Builder
public class SubcontractOrderInput {
    private String supplierCode;
    private LocalDate deliveryDate;
    private String itemCode;
    private BigDecimal quantity;
    private BigDecimal unitPrice;
}
