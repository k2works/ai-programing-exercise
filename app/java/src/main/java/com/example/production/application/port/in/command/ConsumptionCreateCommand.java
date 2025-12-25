package com.example.production.application.port.in.command;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.List;

/**
 * 消費作成コマンド
 */
@Data
@Builder
public class ConsumptionCreateCommand {
    private String receivingNumber;
    private LocalDate consumptionDate;
    private String supplierCode;
    private String remarks;
    private List<ConsumptionDetailCommand> details;
}
