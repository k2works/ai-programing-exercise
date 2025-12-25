package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * ロット構成登録コマンド
 */
@Data
@Builder
public class LotCompositionCreateCommand {
    private String parentLotNumber;
    private String childLotNumber;
    private BigDecimal usedQuantity;
}
