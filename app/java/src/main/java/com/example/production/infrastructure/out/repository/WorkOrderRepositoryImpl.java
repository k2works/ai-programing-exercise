package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import com.example.production.infrastructure.out.mapper.WorkOrderMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 作業指示リポジトリ実装
 */
@Repository
public class WorkOrderRepositoryImpl implements WorkOrderRepository {

    private final WorkOrderMapper workOrderMapper;

    public WorkOrderRepositoryImpl(WorkOrderMapper workOrderMapper) {
        this.workOrderMapper = workOrderMapper;
    }

    @Override
    public void save(WorkOrder workOrder) {
        workOrderMapper.insert(workOrder);
    }

    @Override
    public Optional<WorkOrder> findByWorkOrderNumber(String workOrderNumber) {
        return workOrderMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public List<WorkOrder> findByOrderNumber(String orderNumber) {
        return workOrderMapper.findByOrderNumber(orderNumber);
    }

    @Override
    public List<WorkOrder> findByStatus(WorkOrderStatus status) {
        return workOrderMapper.findByStatus(status);
    }

    @Override
    public Optional<String> findLatestWorkOrderNumber(String prefix) {
        return workOrderMapper.findLatestWorkOrderNumber(prefix);
    }

    @Override
    public void startWork(String workOrderNumber, LocalDate actualStartDate) {
        workOrderMapper.startWork(workOrderNumber, actualStartDate);
    }

    @Override
    public void completeWork(String workOrderNumber, LocalDate actualEndDate) {
        workOrderMapper.completeWork(workOrderNumber, actualEndDate);
    }

    @Override
    public void updateCompletionQuantities(String workOrderNumber, BigDecimal completedQuantity,
                                           BigDecimal goodQuantity, BigDecimal defectQuantity) {
        workOrderMapper.updateCompletionQuantities(workOrderNumber, completedQuantity, goodQuantity, defectQuantity);
    }

    @Override
    public List<WorkOrder> findAll() {
        return workOrderMapper.findAll();
    }

    @Override
    public void deleteAll() {
        workOrderMapper.deleteAll();
    }
}
