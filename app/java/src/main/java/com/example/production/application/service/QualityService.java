package com.example.production.application.service;

import com.example.production.application.port.in.command.*;
import com.example.production.application.port.out.*;
import com.example.production.domain.model.quality.*;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Year;
import java.util.ArrayList;
import java.util.List;

/**
 * 品質管理サービス
 */
@Service
@RequiredArgsConstructor
public class QualityService {

    private final DefectMasterRepository defectMasterRepository;
    private final ShipmentInspectionRepository shipmentInspectionRepository;
    private final LotMasterRepository lotMasterRepository;
    private final LotCompositionRepository lotCompositionRepository;

    /**
     * 欠点マスタを登録する
     */
    @Transactional
    public DefectMaster registerDefect(DefectMasterCreateCommand command) {
        DefectMaster defect = DefectMaster.builder()
                .defectCode(command.getDefectCode())
                .defectDescription(command.getDefectDescription())
                .build();

        defectMasterRepository.save(defect);
        return defect;
    }

    /**
     * 欠点マスタを取得する
     */
    public DefectMaster findDefectByCode(String defectCode) {
        return defectMasterRepository.findByDefectCode(defectCode)
                .orElseThrow(() -> new IllegalArgumentException("欠点マスタが見つかりません: " + defectCode));
    }

    /**
     * 全ての欠点マスタを取得する
     */
    public List<DefectMaster> findAllDefects() {
        return defectMasterRepository.findAll();
    }

    /**
     * 出荷検査を登録する
     */
    @Transactional
    public ShipmentInspection registerShipmentInspection(ShipmentInspectionCreateCommand command) {
        String inspectionNumber = generateShipmentInspectionNumber();

        ShipmentInspection inspection = ShipmentInspection.builder()
                .shipmentInspectionNumber(inspectionNumber)
                .shipmentNumber(command.getShipmentNumber())
                .itemCode(command.getItemCode())
                .inspectionDate(command.getInspectionDate())
                .inspectorCode(command.getInspectorCode())
                .inspectionQuantity(command.getInspectionQuantity())
                .passedQuantity(command.getPassedQuantity())
                .failedQuantity(command.getFailedQuantity())
                .judgment(command.getJudgment())
                .build();

        shipmentInspectionRepository.save(inspection);

        List<ShipmentInspectionResult> results = new ArrayList<>();
        if (command.getResults() != null) {
            for (ShipmentInspectionResultCommand resultCommand : command.getResults()) {
                ShipmentInspectionResult result = ShipmentInspectionResult.builder()
                        .shipmentInspectionNumber(inspectionNumber)
                        .defectCode(resultCommand.getDefectCode())
                        .quantity(resultCommand.getQuantity())
                        .build();
                shipmentInspectionRepository.saveResult(result);
                results.add(result);
            }
        }

        inspection.setResults(results);
        return inspection;
    }

    /**
     * 出荷検査を取得する
     */
    public ShipmentInspection findShipmentInspection(String shipmentInspectionNumber) {
        return shipmentInspectionRepository.findByShipmentInspectionNumberWithResults(shipmentInspectionNumber);
    }

    /**
     * 出荷番号で出荷検査を検索する
     */
    public List<ShipmentInspection> findShipmentInspectionsByShipmentNumber(String shipmentNumber) {
        return shipmentInspectionRepository.findByShipmentNumber(shipmentNumber);
    }

    /**
     * ロットを作成する
     */
    @Transactional
    public LotMaster createLot(LotCreateCommand command) {
        LotMaster lot = LotMaster.builder()
                .lotNumber(command.getLotNumber())
                .itemCode(command.getItemCode())
                .lotType(command.getLotType())
                .productionDate(command.getProductionDate())
                .expirationDate(command.getExpirationDate())
                .quantity(command.getQuantity())
                .build();

        lotMasterRepository.save(lot);
        return lot;
    }

    /**
     * ロットを取得する
     */
    public LotMaster findLot(String lotNumber) {
        return lotMasterRepository.findByLotNumber(lotNumber)
                .orElseThrow(() -> new IllegalArgumentException("ロットが見つかりません: " + lotNumber));
    }

    /**
     * 品目コードでロットを検索する
     */
    public List<LotMaster> findLotsByItemCode(String itemCode) {
        return lotMasterRepository.findByItemCode(itemCode);
    }

    /**
     * ロット構成を登録する（トレーサビリティ）
     */
    @Transactional
    public LotComposition registerLotComposition(LotCompositionCreateCommand command) {
        // 親ロットと子ロットの存在確認
        lotMasterRepository.findByLotNumber(command.getParentLotNumber())
                .orElseThrow(() -> new IllegalArgumentException("親ロットが見つかりません: " + command.getParentLotNumber()));
        lotMasterRepository.findByLotNumber(command.getChildLotNumber())
                .orElseThrow(() -> new IllegalArgumentException("子ロットが見つかりません: " + command.getChildLotNumber()));

        LotComposition composition = LotComposition.builder()
                .parentLotNumber(command.getParentLotNumber())
                .childLotNumber(command.getChildLotNumber())
                .usedQuantity(command.getUsedQuantity())
                .build();

        lotCompositionRepository.save(composition);
        return composition;
    }

    /**
     * 親ロット番号からロット構成を取得する（順方向トレース）
     */
    public List<LotComposition> findLotCompositionsByParent(String parentLotNumber) {
        return lotCompositionRepository.findByParentLotNumber(parentLotNumber);
    }

    /**
     * 子ロット番号からロット構成を取得する（逆方向トレース）
     */
    public List<LotComposition> findLotCompositionsByChild(String childLotNumber) {
        return lotCompositionRepository.findByChildLotNumber(childLotNumber);
    }

    /**
     * 順方向トレース（親ロットから子ロットを辿る）
     */
    public List<LotMaster> traceForward(String parentLotNumber) {
        List<LotMaster> result = new ArrayList<>();
        traceForwardRecursive(parentLotNumber, result);
        return result;
    }

    private void traceForwardRecursive(String lotNumber, List<LotMaster> result) {
        List<LotComposition> compositions = lotCompositionRepository.findByParentLotNumber(lotNumber);
        for (LotComposition composition : compositions) {
            lotMasterRepository.findByLotNumber(composition.getChildLotNumber())
                    .ifPresent(lot -> {
                        result.add(lot);
                        traceForwardRecursive(lot.getLotNumber(), result);
                    });
        }
    }

    /**
     * 逆方向トレース（子ロットから親ロットを辿る）
     */
    public List<LotMaster> traceBackward(String childLotNumber) {
        List<LotMaster> result = new ArrayList<>();
        traceBackwardRecursive(childLotNumber, result);
        return result;
    }

    private void traceBackwardRecursive(String lotNumber, List<LotMaster> result) {
        List<LotComposition> compositions = lotCompositionRepository.findByChildLotNumber(lotNumber);
        for (LotComposition composition : compositions) {
            lotMasterRepository.findByLotNumber(composition.getParentLotNumber())
                    .ifPresent(lot -> {
                        result.add(lot);
                        traceBackwardRecursive(lot.getLotNumber(), result);
                    });
        }
    }

    private String generateShipmentInspectionNumber() {
        int year = Year.now().getValue();
        long count = shipmentInspectionRepository.countByYear(year);
        return String.format("SI-%d-%04d", year, count + 1);
    }
}
