package com.example.production.domain.model.inventory;

import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * 払出明細データエンティティ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class IssueDetail {
    private Long id;
    private String issueNumber;
    private Integer lineNumber;
    private String itemCode;
    private BigDecimal issueQuantity;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
}
