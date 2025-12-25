package com.example.production.application.port.out;

import com.example.production.domain.model.quality.ShipmentInspection;
import com.example.production.domain.model.quality.ShipmentInspectionResult;

import java.util.List;
import java.util.Optional;

/**
 * 出荷検査リポジトリ
 */
public interface ShipmentInspectionRepository {

    void save(ShipmentInspection inspection);

    void saveResult(ShipmentInspectionResult result);

    Optional<ShipmentInspection> findByShipmentInspectionNumber(String shipmentInspectionNumber);

    ShipmentInspection findByShipmentInspectionNumberWithResults(String shipmentInspectionNumber);

    List<ShipmentInspection> findByShipmentNumber(String shipmentNumber);

    List<ShipmentInspectionResult> findResultsByShipmentInspectionNumber(String shipmentInspectionNumber);

    long countByYear(int year);

    void deleteAll();

    void deleteAllResults();
}
