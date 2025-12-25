package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * 払出指示作成コマンド
 */
@Data
@Builder
public class IssueInstructionCreateCommand {
    private String orderNumber;
    private LocalDate instructionDate;
    private String locationCode;
    private String remarks;
    private List<IssueInstructionDetailCommand> details;
}
