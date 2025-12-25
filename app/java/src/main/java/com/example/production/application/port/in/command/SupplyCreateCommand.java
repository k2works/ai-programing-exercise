package com.example.production.application.port.in.command;

import com.example.production.domain.model.subcontract.SupplyType;
import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * 支給作成コマンド
 */
@Data
@Builder
public class SupplyCreateCommand {
    private String purchaseOrderNumber;
    private Integer lineNumber;
    private String supplierCode;
    private LocalDate supplyDate;
    private String supplierPersonCode;
    private SupplyType supplyType;
    private String remarks;
    private List<SupplyDetailCommand> details;
}
