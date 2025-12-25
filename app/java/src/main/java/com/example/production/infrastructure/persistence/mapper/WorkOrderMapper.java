package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Mapper
public interface WorkOrderMapper {
    void insert(WorkOrder workOrder);
    Optional<WorkOrder> findByWorkOrderNumber(String workOrderNumber);
    List<WorkOrder> findByOrderNumber(String orderNumber);
    List<WorkOrder> findByStatus(WorkOrderStatus status);
    Optional<String> findLatestWorkOrderNumber(String prefix);
    void startWork(@Param("workOrderNumber") String workOrderNumber,
                   @Param("actualStartDate") LocalDate actualStartDate);
    void completeWork(@Param("workOrderNumber") String workOrderNumber,
                      @Param("actualEndDate") LocalDate actualEndDate);
    void updateCompletionQuantities(@Param("workOrderNumber") String workOrderNumber,
                                    @Param("completedQuantity") BigDecimal completedQuantity,
                                    @Param("goodQuantity") BigDecimal goodQuantity,
                                    @Param("defectQuantity") BigDecimal defectQuantity);
    List<WorkOrder> findAll();
    void deleteAll();
}
