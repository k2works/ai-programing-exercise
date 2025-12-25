package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 完成実績登録コマンド
 */
@Data
@Builder
public class CompletionResultCreateCommand {
    private String workOrderNumber;
    private LocalDate completionDate;
    private BigDecimal completedQuantity;
    private BigDecimal goodQuantity;
    private BigDecimal defectQuantity;
    private String remarks;
    private String createdBy;
    private List<InspectionResultCommand> inspectionResults;

    /**
     * 検査結果コマンド
     */
    @Data
    @Builder
    public static class InspectionResultCommand {
        private String defectCode;
        private BigDecimal quantity;
    }
}
