package com.example.production.application.service.dto;

import lombok.Builder;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

/**
 * 工数サマリ DTO
 */
@Data
@Builder
public class LaborHoursSummary {
    private String workOrderNumber;
    private BigDecimal totalHours;
    private List<ProcessLaborHours> processHours;
}
