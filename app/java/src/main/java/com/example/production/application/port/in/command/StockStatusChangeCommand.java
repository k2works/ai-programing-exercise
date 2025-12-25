package com.example.production.application.port.in.command;

import com.example.production.domain.model.inventory.StockStatus;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 在庫状態変更コマンド
 */
@Data
@Builder
public class StockStatusChangeCommand {
    private String locationCode;
    private String itemCode;
    private BigDecimal quantity;
    private StockStatus fromStatus;
    private StockStatus toStatus;
}
