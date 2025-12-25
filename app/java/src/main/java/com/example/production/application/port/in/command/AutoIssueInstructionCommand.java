package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

/**
 * 自動払出指示生成コマンド
 */
@Data
@Builder
public class AutoIssueInstructionCommand {
    private String orderNumber;
    private LocalDate instructionDate;
    private String locationCode;
}
