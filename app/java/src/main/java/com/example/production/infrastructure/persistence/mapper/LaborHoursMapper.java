package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.process.LaborHours;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Mapper
public interface LaborHoursMapper {
    void insert(LaborHours laborHours);
    Optional<LaborHours> findByLaborHoursNumber(String laborHoursNumber);
    List<LaborHours> findByWorkOrderNumber(String workOrderNumber);
    BigDecimal sumByWorkOrderAndSequence(@Param("workOrderNumber") String workOrderNumber,
                                         @Param("sequence") Integer sequence);
    BigDecimal sumByWorkOrder(String workOrderNumber);
    BigDecimal sumByEmployee(@Param("employeeCode") String employeeCode,
                             @Param("startDate") LocalDate startDate,
                             @Param("endDate") LocalDate endDate);
    Optional<String> findLatestLaborHoursNumber(String prefix);
    List<LaborHours> findAll();
    void deleteAll();
}
