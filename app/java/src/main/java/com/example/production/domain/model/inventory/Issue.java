package com.example.production.domain.model.inventory;

import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 払出データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Issue {
    private Long id;
    private String issueNumber;
    private String workOrderNumber;
    private Integer routingSequence;
    private String locationCode;
    private LocalDate issueDate;
    private String issuerCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    private List<IssueDetail> details;
}
