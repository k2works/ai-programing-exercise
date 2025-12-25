package com.example.production.application.port.out;

import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 作業指示リポジトリ（Output Port）
 */
public interface WorkOrderRepository {
    void save(WorkOrder workOrder);
    Optional<WorkOrder> findByWorkOrderNumber(String workOrderNumber);
    List<WorkOrder> findByOrderNumber(String orderNumber);
    List<WorkOrder> findByStatus(WorkOrderStatus status);
    Optional<String> findLatestWorkOrderNumber(String prefix);
    void startWork(String workOrderNumber, LocalDate actualStartDate);
    void completeWork(String workOrderNumber, LocalDate actualEndDate);
    void updateCompletionQuantities(String workOrderNumber, BigDecimal completedQuantity,
                                    BigDecimal goodQuantity, BigDecimal defectQuantity);
    void deleteAll();
}
