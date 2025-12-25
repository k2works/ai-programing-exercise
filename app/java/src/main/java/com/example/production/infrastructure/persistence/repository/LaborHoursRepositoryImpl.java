package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.LaborHoursRepository;
import com.example.production.domain.model.process.LaborHours;
import com.example.production.infrastructure.persistence.mapper.LaborHoursMapper;
import org.springframework.stereotype.Repository;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 工数実績リポジトリ実装
 */
@Repository
public class LaborHoursRepositoryImpl implements LaborHoursRepository {

    private final LaborHoursMapper laborHoursMapper;

    public LaborHoursRepositoryImpl(LaborHoursMapper laborHoursMapper) {
        this.laborHoursMapper = laborHoursMapper;
    }

    @Override
    public void save(LaborHours laborHours) {
        laborHoursMapper.insert(laborHours);
    }

    @Override
    public Optional<LaborHours> findByLaborHoursNumber(String laborHoursNumber) {
        return laborHoursMapper.findByLaborHoursNumber(laborHoursNumber);
    }

    @Override
    public List<LaborHours> findByWorkOrderNumber(String workOrderNumber) {
        return laborHoursMapper.findByWorkOrderNumber(workOrderNumber);
    }

    @Override
    public BigDecimal sumByWorkOrderAndSequence(String workOrderNumber, Integer sequence) {
        return laborHoursMapper.sumByWorkOrderAndSequence(workOrderNumber, sequence);
    }

    @Override
    public BigDecimal sumByWorkOrder(String workOrderNumber) {
        return laborHoursMapper.sumByWorkOrder(workOrderNumber);
    }

    @Override
    public BigDecimal sumByEmployee(String employeeCode, LocalDate startDate, LocalDate endDate) {
        return laborHoursMapper.sumByEmployee(employeeCode, startDate, endDate);
    }

    @Override
    public Optional<String> findLatestLaborHoursNumber(String prefix) {
        return laborHoursMapper.findLatestLaborHoursNumber(prefix);
    }

    @Override
    public void deleteAll() {
        laborHoursMapper.deleteAll();
    }
}
