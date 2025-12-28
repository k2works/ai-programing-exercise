package com.example.production.application.port.out;

import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.PlanStatus;

import java.util.List;
import java.util.Optional;

/**
 * 基準生産計画リポジトリ（Output Port）
 */
public interface MpsRepository {
    void save(MasterProductionSchedule mps);
    Optional<MasterProductionSchedule> findById(Integer id);
    Optional<MasterProductionSchedule> findByMpsNumber(String mpsNumber);
    List<MasterProductionSchedule> findByStatus(PlanStatus status);
    List<MasterProductionSchedule> findAll();
    void updateStatus(Integer id, PlanStatus status);
    void deleteAll();
}
