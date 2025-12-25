package com.example.production.application.port.out;

import com.example.production.domain.model.subcontract.Supply;

import java.util.List;
import java.util.Optional;

/**
 * 支給リポジトリ
 */
public interface SupplyRepository {

    void save(Supply supply);

    Optional<Supply> findById(Integer id);

    Optional<Supply> findBySupplyNumber(String supplyNumber);

    List<Supply> findByPurchaseOrderDetail(String purchaseOrderNumber, Integer lineNumber);

    Optional<String> findLatestSupplyNumber(String prefix);

    void deleteAll();
}
