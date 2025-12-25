package com.example.production.application.port.out;

import com.example.production.domain.model.process.WorkOrderDetail;

import java.util.List;
import java.util.Optional;

/**
 * 作業指示明細リポジトリ（Output Port）
 */
public interface WorkOrderDetailRepository {
    void save(WorkOrderDetail detail);
    List<WorkOrderDetail> findByWorkOrderNumber(String workOrderNumber);
    Optional<WorkOrderDetail> findByWorkOrderAndSequence(String workOrderNumber, Integer sequence);
    void deleteAll();
}
