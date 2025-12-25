package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 出荷検査結果コマンド
 */
@Data
@Builder
public class ShipmentInspectionResultCommand {
    private String defectCode;
    private BigDecimal quantity;
}
