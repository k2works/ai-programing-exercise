package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.ShipmentInspectionRepository;
import com.example.production.domain.model.quality.ShipmentInspection;
import com.example.production.domain.model.quality.ShipmentInspectionResult;
import com.example.production.infrastructure.persistence.mapper.ShipmentInspectionMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 出荷検査リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class ShipmentInspectionRepositoryImpl implements ShipmentInspectionRepository {

    private final ShipmentInspectionMapper shipmentInspectionMapper;

    @Override
    public void save(ShipmentInspection inspection) {
        shipmentInspectionMapper.insert(inspection);
    }

    @Override
    public void saveResult(ShipmentInspectionResult result) {
        shipmentInspectionMapper.insertResult(result);
    }

    @Override
    public Optional<ShipmentInspection> findByShipmentInspectionNumber(String shipmentInspectionNumber) {
        return shipmentInspectionMapper.findByShipmentInspectionNumber(shipmentInspectionNumber);
    }

    @Override
    public ShipmentInspection findByShipmentInspectionNumberWithResults(String shipmentInspectionNumber) {
        return shipmentInspectionMapper.findByShipmentInspectionNumberWithResults(shipmentInspectionNumber);
    }

    @Override
    public List<ShipmentInspection> findByShipmentNumber(String shipmentNumber) {
        return shipmentInspectionMapper.findByShipmentNumber(shipmentNumber);
    }

    @Override
    public List<ShipmentInspectionResult> findResultsByShipmentInspectionNumber(String shipmentInspectionNumber) {
        return shipmentInspectionMapper.findResultsByShipmentInspectionNumber(shipmentInspectionNumber);
    }

    @Override
    public long countByYear(int year) {
        return shipmentInspectionMapper.countByYear(year);
    }

    @Override
    public void deleteAll() {
        shipmentInspectionMapper.deleteAll();
    }

    @Override
    public void deleteAllResults() {
        shipmentInspectionMapper.deleteAllResults();
    }
}
