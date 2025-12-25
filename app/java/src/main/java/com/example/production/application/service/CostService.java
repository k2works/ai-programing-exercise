package com.example.production.application.service;

import com.example.production.application.port.in.command.ActualCostCalculateCommand;
import com.example.production.application.port.in.command.StandardCostCreateCommand;
import com.example.production.application.port.out.ActualCostRepository;
import com.example.production.application.port.out.CostVarianceRepository;
import com.example.production.application.port.out.StandardCostRepository;
import com.example.production.domain.model.cost.ActualCost;
import com.example.production.domain.model.cost.CostVariance;
import com.example.production.domain.model.cost.StandardCost;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;

/**
 * 製造原価管理サービス
 */
@Service
@RequiredArgsConstructor
public class CostService {

    private final StandardCostRepository standardCostRepository;
    private final ActualCostRepository actualCostRepository;
    private final CostVarianceRepository costVarianceRepository;

    /**
     * 標準原価を登録する
     */
    @Transactional
    public StandardCost registerStandardCost(StandardCostCreateCommand command) {
        // 標準製造原価を計算（材料費 + 労務費 + 経費）
        BigDecimal standardManufacturingCost = command.getStandardMaterialCost()
                .add(command.getStandardLaborCost())
                .add(command.getStandardExpense());

        StandardCost standardCost = StandardCost.builder()
                .itemCode(command.getItemCode())
                .effectiveStartDate(command.getEffectiveStartDate())
                .effectiveEndDate(command.getEffectiveEndDate())
                .standardMaterialCost(command.getStandardMaterialCost())
                .standardLaborCost(command.getStandardLaborCost())
                .standardExpense(command.getStandardExpense())
                .standardManufacturingCost(standardManufacturingCost)
                .build();

        standardCostRepository.save(standardCost);
        return standardCost;
    }

    /**
     * 品目コードと日付で標準原価を取得する
     */
    public StandardCost findStandardCost(String itemCode, LocalDate date) {
        return standardCostRepository.findByItemCodeAndDate(itemCode, date)
                .orElseThrow(() -> new IllegalArgumentException(
                        "標準原価が見つかりません: 品目コード=" + itemCode + ", 日付=" + date));
    }

    /**
     * 品目コードで標準原価を検索する
     */
    public List<StandardCost> findStandardCostsByItemCode(String itemCode) {
        return standardCostRepository.findByItemCode(itemCode);
    }

    /**
     * 全ての標準原価を取得する
     */
    public List<StandardCost> findAllStandardCosts() {
        return standardCostRepository.findAll();
    }

    /**
     * 実際原価を計算して登録する
     */
    @Transactional
    public ActualCost calculateAndRegisterActualCost(ActualCostCalculateCommand command) {
        // 実際製造原価を計算（材料費 + 労務費 + 経費）
        BigDecimal actualManufacturingCost = command.getActualMaterialCost()
                .add(command.getActualLaborCost())
                .add(command.getActualExpense());

        // 単位原価を計算（実際製造原価 / 完成数量）
        BigDecimal unitCost = actualManufacturingCost
                .divide(command.getCompletedQuantity(), 4, RoundingMode.HALF_UP);

        ActualCost actualCost = ActualCost.builder()
                .workOrderNumber(command.getWorkOrderNumber())
                .itemCode(command.getItemCode())
                .completedQuantity(command.getCompletedQuantity())
                .actualMaterialCost(command.getActualMaterialCost())
                .actualLaborCost(command.getActualLaborCost())
                .actualExpense(command.getActualExpense())
                .actualManufacturingCost(actualManufacturingCost)
                .unitCost(unitCost)
                .build();

        actualCostRepository.save(actualCost);
        return actualCost;
    }

    /**
     * 作業指示番号で実際原価を取得する
     */
    public ActualCost findActualCost(String workOrderNumber) {
        return actualCostRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException(
                        "実際原価が見つかりません: " + workOrderNumber));
    }

    /**
     * 品目コードで実際原価を検索する
     */
    public List<ActualCost> findActualCostsByItemCode(String itemCode) {
        return actualCostRepository.findByItemCode(itemCode);
    }

    /**
     * 全ての実際原価を取得する
     */
    public List<ActualCost> findAllActualCosts() {
        return actualCostRepository.findAll();
    }

    /**
     * 原価差異を計算して登録する
     */
    @Transactional
    public CostVariance calculateAndRegisterCostVariance(String workOrderNumber, LocalDate calculationDate) {
        // 実際原価を取得
        ActualCost actualCost = findActualCost(workOrderNumber);

        // 標準原価を取得
        StandardCost standardCost = findStandardCost(actualCost.getItemCode(), calculationDate);

        // 標準原価に完成数量を乗じて期待原価を算出
        BigDecimal expectedMaterialCost = standardCost.getStandardMaterialCost()
                .multiply(actualCost.getCompletedQuantity());
        BigDecimal expectedLaborCost = standardCost.getStandardLaborCost()
                .multiply(actualCost.getCompletedQuantity());
        BigDecimal expectedExpense = standardCost.getStandardExpense()
                .multiply(actualCost.getCompletedQuantity());

        // 差異を計算（実際 - 標準）
        BigDecimal materialCostVariance = actualCost.getActualMaterialCost()
                .subtract(expectedMaterialCost);
        BigDecimal laborCostVariance = actualCost.getActualLaborCost()
                .subtract(expectedLaborCost);
        BigDecimal expenseVariance = actualCost.getActualExpense()
                .subtract(expectedExpense);
        BigDecimal totalVariance = materialCostVariance
                .add(laborCostVariance)
                .add(expenseVariance);

        CostVariance costVariance = CostVariance.builder()
                .workOrderNumber(workOrderNumber)
                .itemCode(actualCost.getItemCode())
                .materialCostVariance(materialCostVariance)
                .laborCostVariance(laborCostVariance)
                .expenseVariance(expenseVariance)
                .totalVariance(totalVariance)
                .build();

        costVarianceRepository.save(costVariance);
        return costVariance;
    }

    /**
     * 作業指示番号で原価差異を取得する
     */
    public CostVariance findCostVariance(String workOrderNumber) {
        return costVarianceRepository.findByWorkOrderNumber(workOrderNumber)
                .orElseThrow(() -> new IllegalArgumentException(
                        "原価差異が見つかりません: " + workOrderNumber));
    }

    /**
     * 品目コードで原価差異を検索する
     */
    public List<CostVariance> findCostVariancesByItemCode(String itemCode) {
        return costVarianceRepository.findByItemCode(itemCode);
    }

    /**
     * 全ての原価差異を取得する
     */
    public List<CostVariance> findAllCostVariances() {
        return costVarianceRepository.findAll();
    }
}
