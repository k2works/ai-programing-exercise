package com.example.production.domain.model.inventory;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 払出指示明細データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IssueInstructionDetail {
    private Long id;
    private String instructionNumber;
    private Integer lineNumber;
    private String itemCode;
    private Integer routingSequence;
    private BigDecimal issueQuantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
