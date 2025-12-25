package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.Acceptance;

import java.util.List;
import java.util.Optional;

/**
 * 検収リポジトリ
 */
public interface AcceptanceRepository {

    void save(Acceptance acceptance);

    Optional<Acceptance> findById(Integer id);

    Optional<Acceptance> findByAcceptanceNumber(String acceptanceNumber);

    List<Acceptance> findByInspectionNumber(String inspectionNumber);

    List<Acceptance> findByPurchaseOrderNumber(String purchaseOrderNumber);

    Optional<String> findLatestAcceptanceNumber(String prefix);

    void deleteAll();
}
