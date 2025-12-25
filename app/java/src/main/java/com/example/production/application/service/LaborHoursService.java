package com.example.production.application.service;

import com.example.production.application.port.in.command.LaborHoursCreateCommand;
import com.example.production.application.port.out.LaborHoursRepository;
import com.example.production.application.port.out.ProcessRepository;
import com.example.production.application.port.out.WorkOrderDetailRepository;
import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.application.service.dto.LaborHoursSummary;
import com.example.production.application.service.dto.ProcessLaborHours;
import com.example.production.domain.model.process.LaborHours;
import com.example.production.domain.model.process.Process;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderDetail;
import com.example.production.domain.model.process.WorkOrderStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * 工数実績サービス
 */
@Service
@RequiredArgsConstructor
public class LaborHoursService {

    private final LaborHoursRepository laborHoursRepository;
    private final WorkOrderRepository workOrderRepository;
    private final WorkOrderDetailRepository workOrderDetailRepository;
    private final ProcessRepository processRepository;

    /**
     * 工数実績番号を生成する
     */
    private String generateLaborHoursNumber(LocalDate workDate) {
        String prefix = "LH-" + workDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        return laborHoursRepository.findLatestLaborHoursNumber(prefix)
                .map(latest -> {
                    int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
                    return prefix + String.format("%04d", currentSequence + 1);
                })
                .orElse(prefix + "0001");
    }

    /**
     * 工数実績を登録する
     */
    @Transactional
    public LaborHours createLaborHours(LaborHoursCreateCommand command) {
        // 作業指示を取得
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(command.getWorkOrderNumber())
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + command.getWorkOrderNumber()));

        // 作業指示の状態を確認
        if (workOrder.getStatus() != WorkOrderStatus.IN_PROGRESS) {
            throw new IllegalStateException("Only IN_PROGRESS work orders can record labor hours");
        }

        // 作業指示明細を取得
        WorkOrderDetail detail = workOrderDetailRepository.findByWorkOrderAndSequence(
                        command.getWorkOrderNumber(), command.getSequence())
                .orElseThrow(() -> new IllegalArgumentException("Work order detail not found for sequence: " + command.getSequence()));

        String laborHoursNumber = generateLaborHoursNumber(command.getWorkDate());

        // 工数実績を作成
        LaborHours laborHours = LaborHours.builder()
                .laborHoursNumber(laborHoursNumber)
                .workOrderNumber(command.getWorkOrderNumber())
                .itemCode(workOrder.getItemCode())
                .sequence(command.getSequence())
                .processCode(detail.getProcessCode())
                .departmentCode(command.getDepartmentCode())
                .employeeCode(command.getEmployeeCode())
                .workDate(command.getWorkDate())
                .hours(command.getHours())
                .remarks(command.getRemarks())
                .createdBy(command.getCreatedBy())
                .updatedBy(command.getCreatedBy())
                .build();
        laborHoursRepository.save(laborHours);

        return laborHours;
    }

    /**
     * 工数実績番号で検索する
     */
    public LaborHours findByLaborHoursNumber(String laborHoursNumber) {
        return laborHoursRepository.findByLaborHoursNumber(laborHoursNumber).orElse(null);
    }

    /**
     * 作業指示番号で工数実績を検索する
     */
    public List<LaborHours> findByWorkOrderNumber(String workOrderNumber) {
        return laborHoursRepository.findByWorkOrderNumber(workOrderNumber);
    }

    /**
     * 工順別の工数合計を取得する
     */
    public BigDecimal getTotalHoursBySequence(String workOrderNumber, Integer sequence) {
        return laborHoursRepository.sumByWorkOrderAndSequence(workOrderNumber, sequence);
    }

    /**
     * 作業指示の工数サマリを取得する
     */
    public LaborHoursSummary getSummary(String workOrderNumber) {
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + workOrderNumber));

        List<WorkOrderDetail> details = workOrderDetailRepository.findByWorkOrderNumber(workOrderNumber);
        List<ProcessLaborHours> processHoursList = new ArrayList<>();
        BigDecimal totalHours = BigDecimal.ZERO;

        for (WorkOrderDetail detail : details) {
            BigDecimal hours = laborHoursRepository.sumByWorkOrderAndSequence(workOrderNumber, detail.getSequence());
            Process process = processRepository.findByProcessCode(detail.getProcessCode()).orElse(null);

            processHoursList.add(ProcessLaborHours.builder()
                    .sequence(detail.getSequence())
                    .processCode(detail.getProcessCode())
                    .processName(process != null ? process.getProcessName() : "")
                    .hours(hours)
                    .build());

            totalHours = totalHours.add(hours);
        }

        return LaborHoursSummary.builder()
                .workOrderNumber(workOrderNumber)
                .totalHours(totalHours)
                .processHours(processHoursList)
                .build();
    }

    /**
     * 担当者別の工数を取得する
     */
    public BigDecimal getTotalHoursByEmployee(String employeeCode, LocalDate startDate, LocalDate endDate) {
        return laborHoursRepository.sumByEmployee(employeeCode, startDate, endDate);
    }
}
