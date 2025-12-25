package com.example.production.domain.model.process;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.plan.Order;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WorkOrder {
    private Integer id;
    private String workOrderNumber;
    private String orderNumber;
    private LocalDate workOrderDate;
    private String itemCode;
    private BigDecimal orderQuantity;
    private String locationCode;
    private LocalDate plannedStartDate;
    private LocalDate plannedEndDate;
    private LocalDate actualStartDate;
    private LocalDate actualEndDate;
    @Builder.Default
    private BigDecimal completedQuantity = BigDecimal.ZERO;
    @Builder.Default
    private BigDecimal totalGoodQuantity = BigDecimal.ZERO;
    @Builder.Default
    private BigDecimal totalDefectQuantity = BigDecimal.ZERO;
    @Builder.Default
    private WorkOrderStatus status = WorkOrderStatus.NOT_STARTED;
    @Builder.Default
    private Boolean completedFlag = false;
    private String remarks;
    private LocalDateTime createdAt;
    private String createdBy;
    private LocalDateTime updatedAt;
    private String updatedBy;

    // リレーション
    private Order order;
    private Item item;
    private Location location;
    private List<WorkOrderDetail> details;
}
