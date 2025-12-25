package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 消費明細コマンド
 */
@Data
@Builder
public class ConsumptionDetailCommand {
    private String itemCode;
    private BigDecimal quantity;
    private String remarks;
}
