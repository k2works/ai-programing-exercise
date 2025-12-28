package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.Inspection;

import java.util.List;
import java.util.Optional;

/**
 * 受入検査リポジトリ
 */
public interface InspectionRepository {

    void save(Inspection inspection);

    Optional<Inspection> findById(Integer id);

    Optional<Inspection> findByInspectionNumber(String inspectionNumber);

    List<Inspection> findByReceivingNumber(String receivingNumber);

    List<Inspection> findAll();

    Optional<String> findLatestInspectionNumber(String prefix);

    void deleteAll();
}
