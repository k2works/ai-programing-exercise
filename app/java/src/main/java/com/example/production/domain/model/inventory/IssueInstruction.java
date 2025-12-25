package com.example.production.domain.model.inventory;

import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 払出指示データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IssueInstruction {
    private Long id;
    private String instructionNumber;
    private String orderNumber;
    private LocalDate instructionDate;
    private String locationCode;
    private String remarks;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    private List<IssueInstructionDetail> details;
}
