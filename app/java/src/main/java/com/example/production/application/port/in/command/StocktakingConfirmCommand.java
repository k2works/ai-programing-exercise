package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

/**
 * 棚卸確定コマンド
 */
@Data
@Builder
public class StocktakingConfirmCommand {
    private String stocktakingNumber;
    private String adjustmentReasonCode;
    private String adjusterCode;
}
