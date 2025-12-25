package com.example.production.application.port.out;

import com.example.production.domain.model.process.LaborHours;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 工数実績リポジトリ（Output Port）
 */
public interface LaborHoursRepository {
    void save(LaborHours laborHours);
    Optional<LaborHours> findByLaborHoursNumber(String laborHoursNumber);
    List<LaborHours> findByWorkOrderNumber(String workOrderNumber);
    BigDecimal sumByWorkOrderAndSequence(String workOrderNumber, Integer sequence);
    BigDecimal sumByWorkOrder(String workOrderNumber);
    BigDecimal sumByEmployee(String employeeCode, LocalDate startDate, LocalDate endDate);
    Optional<String> findLatestLaborHoursNumber(String prefix);
    void deleteAll();
}
