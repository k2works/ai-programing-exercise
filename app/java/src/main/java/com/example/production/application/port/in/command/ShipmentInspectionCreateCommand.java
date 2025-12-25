package com.example.production.application.port.in.command;

import com.example.production.domain.model.quality.InspectionJudgment;
import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * 出荷検査登録コマンド
 */
@Data
@Builder
public class ShipmentInspectionCreateCommand {
    private String shipmentNumber;
    private String itemCode;
    private LocalDate inspectionDate;
    private String inspectorCode;
    private BigDecimal inspectionQuantity;
    private BigDecimal passedQuantity;
    private BigDecimal failedQuantity;
    private InspectionJudgment judgment;
    private List<ShipmentInspectionResultCommand> results;
}
