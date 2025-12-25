package com.example.production.domain.model.process;

import com.example.production.domain.model.item.Item;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CompletionResult {
    private Integer id;
    private String completionResultNumber;
    private String workOrderNumber;
    private String itemCode;
    private LocalDate completionDate;
    private BigDecimal completedQuantity;
    private BigDecimal goodQuantity;
    private BigDecimal defectQuantity;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // リレーション
    private WorkOrder workOrder;
    private Item item;
    private List<CompletionInspectionResult> inspectionResults;
}
