package com.example.production.application.port.in.command;

import com.example.production.domain.model.inventory.StockStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 在庫増減コマンド
 */
@Data
@Builder
public class StockChangeCommand {
    private String locationCode;
    private String itemCode;
    private BigDecimal quantity;
    private StockStatus stockStatus;
}
