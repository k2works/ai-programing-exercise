package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.util.List;

/**
 * 実棚入力コマンド
 */
@Data
@Builder
public class ActualCountInputCommand {
    private String stocktakingNumber;
    private List<ActualCountDetailCommand> details;
}
