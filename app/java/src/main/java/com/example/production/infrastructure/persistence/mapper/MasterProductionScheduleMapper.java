package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.PlanStatus;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface MasterProductionScheduleMapper {
    void insert(MasterProductionSchedule mps);

    Optional<MasterProductionSchedule> findById(Integer id);

    Optional<MasterProductionSchedule> findByMpsNumber(String mpsNumber);

    List<MasterProductionSchedule> findByStatus(PlanStatus status);

    void updateStatus(@Param("id") Integer id, @Param("status") PlanStatus status);

    void deleteAll();
}
