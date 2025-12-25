package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 外注発注コマンド
 */
@Data
@Builder
public class SubcontractOrderCommand {
    private String supplierCode;
    private LocalDate deliveryDate;
    private String itemCode;
    private BigDecimal quantity;
    private BigDecimal unitPrice;
}
