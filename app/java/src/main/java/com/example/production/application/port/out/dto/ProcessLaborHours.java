package com.example.production.application.port.out.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 工程別工数 DTO
 */
@Data
@Builder
public class ProcessLaborHours {
    private Integer sequence;
    private String processCode;
    private String processName;
    private BigDecimal hours;
}
