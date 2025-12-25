package com.example.production.domain.model.process;

import com.example.production.domain.model.purchase.Defect;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CompletionInspectionResult {
    private Integer id;
    private String completionResultNumber;
    private String defectCode;
    private BigDecimal quantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // リレーション
    private CompletionResult completionResult;
    private Defect defect;
}
