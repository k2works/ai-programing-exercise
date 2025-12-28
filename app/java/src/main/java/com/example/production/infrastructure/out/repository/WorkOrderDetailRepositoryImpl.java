package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.WorkOrderDetailRepository;
import com.example.production.domain.model.process.WorkOrderDetail;
import com.example.production.infrastructure.out.mapper.WorkOrderDetailMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 作業指示明細リポジトリ実装
 */
@Repository
public class WorkOrderDetailRepositoryImpl implements WorkOrderDetailRepository {

    private final WorkOrderDetailMapper workOrderDetailMapper;

    public WorkOrderDetailRepositoryImpl(WorkOrderDetailMapper workOrderDetailMapper) {
        this.workOrderDetailMapper = workOrderDetailMapper;
    }

    @Override
    public void save(WorkOrderDetail detail) {
        workOrderDetailMapper.insert(detail);
    }

    @Override
    public List<WorkOrderDetail> findByWorkOrderNumber(String workOrderNumber) {
        return workOrderDetailMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public Optional<WorkOrderDetail> findByWorkOrderAndSequence(String workOrderNumber, Integer sequence) {
        return workOrderDetailMapper.findByWorkOrderAndSequence(workOrderNumber, sequence);
    }

    @Override
    public void deleteAll() {
        workOrderDetailMapper.deleteAll();
    }
}
