package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 払出指示明細コマンド
 */
@Data
@Builder
public class IssueInstructionDetailCommand {
    private String itemCode;
    private Integer routingSequence;
    private BigDecimal issueQuantity;
}
