package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.quality.ShipmentInspection;
import com.example.production.domain.model.quality.ShipmentInspectionResult;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 出荷検査マッパー
 */
@Mapper
public interface ShipmentInspectionMapper {

    void insert(ShipmentInspection inspection);

    void insertResult(ShipmentInspectionResult result);

    Optional<ShipmentInspection> findByShipmentInspectionNumber(
            @Param("shipmentInspectionNumber") String shipmentInspectionNumber);

    ShipmentInspection findByShipmentInspectionNumberWithResults(
            @Param("shipmentInspectionNumber") String shipmentInspectionNumber);

    List<ShipmentInspection> findByShipmentNumber(@Param("shipmentNumber") String shipmentNumber);

    List<ShipmentInspectionResult> findResultsByShipmentInspectionNumber(
            @Param("shipmentInspectionNumber") String shipmentInspectionNumber);

    long countByYear(@Param("year") int year);

    void deleteAllResults();

    void deleteAll();
}
