package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * 払出実行コマンド
 */
@Data
@Builder
public class IssueExecuteCommand {
    private String instructionNumber;
    private String workOrderNumber;
    private Integer routingSequence;
    private String locationCode;
    private LocalDate issueDate;
    private String issuerCode;
    private List<IssueDetailCommand> details;
}
