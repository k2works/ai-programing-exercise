package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

/**
 * 棚卸表発行コマンド
 */
@Data
@Builder
public class StocktakingIssueCommand {
    private String locationCode;
    private LocalDate stocktakingDate;
}
