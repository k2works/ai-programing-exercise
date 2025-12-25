package com.example.production.application.service;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * 消費作成入力DTO
 */
@Data
@Builder
public class ConsumptionCreateInput {
    private String receivingNumber;
    private LocalDate consumptionDate;
    private String supplierCode;
    private String remarks;
    private List<ConsumptionDetailInput> details;
}
