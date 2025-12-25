package com.example.production.application.service;

import com.example.production.domain.model.subcontract.SupplyType;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * 支給作成入力DTO
 */
@Data
@Builder
public class SupplyCreateInput {
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private String supplierCode;
    private LocalDate supplyDate;
    private String supplierPersonCode;
    private SupplyType supplyType;
    private String remarks;
    private List<SupplyDetailInput> details;
}
