package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.process.WorkOrderDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface WorkOrderDetailMapper {
    void insert(WorkOrderDetail detail);
    List<WorkOrderDetail> findByWorkOrderNumber(String workOrderNumber);
    Optional<WorkOrderDetail> findByWorkOrderAndSequence(@Param("workOrderNumber") String workOrderNumber,
                                                         @Param("sequence") Integer sequence);
    void deleteAll();
}
