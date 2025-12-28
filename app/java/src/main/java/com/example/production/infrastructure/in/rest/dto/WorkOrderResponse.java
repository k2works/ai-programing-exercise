package com.example.production.infrastructure.in.rest.dto;

import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderDetail;
import com.example.production.domain.model.process.WorkOrderStatus;
import lombok.Builder;
import lombok.Value;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Collections;
import java.util.List;

/**
 * 作業指示レスポンス
 */
@Value
@Builder
public class WorkOrderResponse {
    String workOrderNumber;
    String orderNumber;
    LocalDate workOrderDate;
    String itemCode;
    BigDecimal orderQuantity;
    String locationCode;
    LocalDate plannedStartDate;
    LocalDate plannedEndDate;
    LocalDate actualStartDate;
    LocalDate actualEndDate;
    BigDecimal completedQuantity;
    BigDecimal totalGoodQuantity;
    BigDecimal totalDefectQuantity;
    WorkOrderStatus status;
    Boolean completedFlag;
    String remarks;
    List<WorkOrderDetailResponse> details;

    public static WorkOrderResponse from(WorkOrder workOrder) {
        return WorkOrderResponse.builder()
                .workOrderNumber(workOrder.getWorkOrderNumber())
                .orderNumber(workOrder.getOrderNumber())
                .workOrderDate(workOrder.getWorkOrderDate())
                .itemCode(workOrder.getItemCode())
                .orderQuantity(workOrder.getOrderQuantity())
                .locationCode(workOrder.getLocationCode())
                .plannedStartDate(workOrder.getPlannedStartDate())
                .plannedEndDate(workOrder.getPlannedEndDate())
                .actualStartDate(workOrder.getActualStartDate())
                .actualEndDate(workOrder.getActualEndDate())
                .completedQuantity(workOrder.getCompletedQuantity())
                .totalGoodQuantity(workOrder.getTotalGoodQuantity())
                .totalDefectQuantity(workOrder.getTotalDefectQuantity())
                .status(workOrder.getStatus())
                .completedFlag(workOrder.getCompletedFlag())
                .remarks(workOrder.getRemarks())
                .details(workOrder.getDetails() != null
                        ? workOrder.getDetails().stream()
                            .map(WorkOrderDetailResponse::from)
                            .toList()
                        : Collections.emptyList())
                .build();
    }

    @Value
    @Builder
    public static class WorkOrderDetailResponse {
        Integer sequence;
        String processCode;

        public static WorkOrderDetailResponse from(WorkOrderDetail detail) {
            return WorkOrderDetailResponse.builder()
                    .sequence(detail.getSequence())
                    .processCode(detail.getProcessCode())
                    .build();
        }
    }
}
