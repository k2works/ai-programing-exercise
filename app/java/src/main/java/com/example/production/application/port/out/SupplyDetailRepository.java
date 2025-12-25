package com.example.production.application.port.out;

import com.example.production.domain.model.subcontract.SupplyDetail;

import java.util.List;
import java.util.Optional;

/**
 * 支給明細リポジトリ
 */
public interface SupplyDetailRepository {

    void save(SupplyDetail detail);

    Optional<SupplyDetail> findById(Integer id);

    List<SupplyDetail> findBySupplyNumber(String supplyNumber);

    void deleteAll();
}
