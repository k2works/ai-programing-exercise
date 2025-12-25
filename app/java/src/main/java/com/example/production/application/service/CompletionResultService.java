package com.example.production.application.service;

import com.example.production.application.port.in.command.CompletionResultCreateCommand;
import com.example.production.application.port.out.CompletionInspectionResultRepository;
import com.example.production.application.port.out.CompletionResultRepository;
import com.example.production.application.port.out.WorkOrderRepository;
import com.example.production.domain.model.process.CompletionInspectionResult;
import com.example.production.domain.model.process.CompletionResult;
import com.example.production.domain.model.process.WorkOrder;
import com.example.production.domain.model.process.WorkOrderStatus;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * 完成実績サービス
 */
@Service
@RequiredArgsConstructor
public class CompletionResultService {

    private final CompletionResultRepository completionResultRepository;
    private final CompletionInspectionResultRepository completionInspectionResultRepository;
    private final WorkOrderRepository workOrderRepository;

    /**
     * 完成実績番号を生成する
     */
    private String generateCompletionResultNumber(LocalDate completionDate) {
        String prefix = "CR-" + completionDate.format(DateTimeFormatter.ofPattern("yyyyMM")) + "-";
        return completionResultRepository.findLatestCompletionResultNumber(prefix)
                .map(latest -> {
                    int currentSequence = Integer.parseInt(latest.substring(latest.length() - 4));
                    return prefix + String.format("%04d", currentSequence + 1);
                })
                .orElse(prefix + "0001");
    }

    /**
     * 完成実績を登録する
     */
    @Transactional
    public CompletionResult createCompletionResult(CompletionResultCreateCommand command) {
        // 作業指示を取得
        WorkOrder workOrder = workOrderRepository.findByWorkOrderNumber(command.getWorkOrderNumber())
                .orElseThrow(() -> new IllegalArgumentException("Work order not found: " + command.getWorkOrderNumber()));

        // 作業指示の状態を確認
        if (workOrder.getStatus() != WorkOrderStatus.IN_PROGRESS) {
            throw new IllegalStateException("Only IN_PROGRESS work orders can record completion results");
        }

        String completionResultNumber = generateCompletionResultNumber(command.getCompletionDate());

        // 完成実績を作成
        CompletionResult completionResult = CompletionResult.builder()
                .completionResultNumber(completionResultNumber)
                .workOrderNumber(command.getWorkOrderNumber())
                .itemCode(workOrder.getItemCode())
                .completionDate(command.getCompletionDate())
                .completedQuantity(command.getCompletedQuantity())
                .goodQuantity(command.getGoodQuantity())
                .defectQuantity(command.getDefectQuantity())
                .remarks(command.getRemarks())
                .createdBy(command.getCreatedBy())
                .updatedBy(command.getCreatedBy())
                .build();
        completionResultRepository.save(completionResult);

        // 検査結果を登録
        List<CompletionInspectionResult> inspectionResults = new ArrayList<>();
        if (command.getInspectionResults() != null) {
            for (CompletionResultCreateCommand.InspectionResultCommand inspCmd : command.getInspectionResults()) {
                CompletionInspectionResult inspectionResult = CompletionInspectionResult.builder()
                        .completionResultNumber(completionResultNumber)
                        .defectCode(inspCmd.getDefectCode())
                        .quantity(inspCmd.getQuantity())
                        .build();
                completionInspectionResultRepository.save(inspectionResult);
                inspectionResults.add(inspectionResult);
            }
        }

        // 作業指示の完成数量を更新
        workOrderRepository.updateCompletionQuantities(
                command.getWorkOrderNumber(),
                workOrder.getCompletedQuantity().add(command.getCompletedQuantity()),
                workOrder.getTotalGoodQuantity().add(command.getGoodQuantity()),
                workOrder.getTotalDefectQuantity().add(command.getDefectQuantity())
        );

        completionResult.setInspectionResults(inspectionResults);
        return completionResult;
    }

    /**
     * 完成実績番号で検索する
     */
    public CompletionResult findByCompletionResultNumber(String completionResultNumber) {
        CompletionResult result = completionResultRepository.findByCompletionResultNumber(completionResultNumber)
                .orElse(null);
        if (result != null) {
            result.setInspectionResults(completionInspectionResultRepository.findByCompletionResultNumber(completionResultNumber));
        }
        return result;
    }

    /**
     * 作業指示番号で完成実績を検索する
     */
    public List<CompletionResult> findByWorkOrderNumber(String workOrderNumber) {
        List<CompletionResult> results = completionResultRepository.findByWorkOrderNumber(workOrderNumber);
        for (CompletionResult result : results) {
            result.setInspectionResults(completionInspectionResultRepository.findByCompletionResultNumber(result.getCompletionResultNumber()));
        }
        return results;
    }
}
