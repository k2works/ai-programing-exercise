package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.MpsRepository;
import com.example.production.domain.model.plan.MasterProductionSchedule;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.infrastructure.persistence.mapper.MasterProductionScheduleMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 基準生産計画リポジトリ実装
 */
@Repository
public class MpsRepositoryImpl implements MpsRepository {

    private final MasterProductionScheduleMapper mpsMapper;

    public MpsRepositoryImpl(MasterProductionScheduleMapper mpsMapper) {
        this.mpsMapper = mpsMapper;
    }

    @Override
    public void save(MasterProductionSchedule mps) {
        mpsMapper.insert(mps);
    }

    @Override
    public Optional<MasterProductionSchedule> findById(Integer id) {
        return mpsMapper.findById(id);
    }

    @Override
    public Optional<MasterProductionSchedule> findByMpsNumber(String mpsNumber) {
        return mpsMapper.findByMpsNumber(mpsNumber);
    }

    @Override
    public List<MasterProductionSchedule> findByStatus(PlanStatus status) {
        return mpsMapper.findByStatus(status);
    }

    @Override
    public List<MasterProductionSchedule> findAll() {
        return mpsMapper.findAll();
    }

    @Override
    public void update(MasterProductionSchedule mps) {
        mpsMapper.update(mps);
    }

    @Override
    public void updateStatus(Integer id, PlanStatus status) {
        mpsMapper.updateStatus(id, status);
    }

    @Override
    public void deleteAll() {
        mpsMapper.deleteAll();
    }
}
