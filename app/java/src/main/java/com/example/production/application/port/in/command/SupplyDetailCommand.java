package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 支給明細コマンド
 */
@Data
@Builder
public class SupplyDetailCommand {
    private String itemCode;
    private BigDecimal quantity;
    private BigDecimal unitPrice;
    private String remarks;
}
