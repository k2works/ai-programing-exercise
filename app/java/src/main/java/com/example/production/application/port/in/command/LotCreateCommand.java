package com.example.production.application.port.in.command;

import com.example.production.domain.model.quality.LotType;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * ロット作成コマンド
 */
@Data
@Builder
public class LotCreateCommand {
    private String lotNumber;
    private String itemCode;
    private LotType lotType;
    private LocalDate productionDate;
    private LocalDate expirationDate;
    private BigDecimal quantity;
}
