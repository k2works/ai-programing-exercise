package com.example.production.domain.model.process;

import lombok.*;

import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WorkOrderDetail {
    private Integer id;
    private String workOrderNumber;
    private Integer sequence;
    private String processCode;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // リレーション
    private WorkOrder workOrder;
    private Process process;
}
