package com.example.production.domain.model.subcontract;

import lombok.*;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

/**
 * 消費データ
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Consumption {
    private Integer id;
    private String consumptionNumber;
    private String receivingNumber;
    private LocalDate consumptionDate;
    private String supplierCode;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // 明細リスト（リレーション）
    private List<ConsumptionDetail> details;
}
