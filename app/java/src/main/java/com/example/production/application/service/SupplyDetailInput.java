package com.example.production.application.service;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 支給明細入力DTO
 */
@Data
@Builder
public class SupplyDetailInput {
    private String itemCode;
    private BigDecimal quantity;
    private BigDecimal unitPrice;
    private String remarks;
}
