package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

/**
 * 標準原価登録コマンド
 */
@Data
@Builder
public class StandardCostCreateCommand {
    private String itemCode;
    private LocalDate effectiveStartDate;
    private LocalDate effectiveEndDate;
    private BigDecimal standardMaterialCost;
    private BigDecimal standardLaborCost;
    private BigDecimal standardExpense;
}
