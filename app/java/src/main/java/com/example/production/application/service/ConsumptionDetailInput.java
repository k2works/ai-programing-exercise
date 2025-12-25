package com.example.production.application.service;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 消費明細入力DTO
 */
@Data
@Builder
public class ConsumptionDetailInput {
    private String itemCode;
    private BigDecimal quantity;
    private String remarks;
}
