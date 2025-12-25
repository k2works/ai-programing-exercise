package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 実棚入力明細コマンド
 */
@Data
@Builder
public class ActualCountDetailCommand {
    private String itemCode;
    private BigDecimal actualQuantity;
}
