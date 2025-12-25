package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 払出明細コマンド
 */
@Data
@Builder
public class IssueDetailCommand {
    private String itemCode;
    private BigDecimal issueQuantity;
}
