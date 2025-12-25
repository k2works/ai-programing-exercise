package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.UnitPrice;

import java.time.LocalDate;
import java.util.Optional;

/**
 * 単価マスタリポジトリ（Output Port）
 */
public interface UnitPriceRepository {
    void save(UnitPrice unitPrice);

    Optional<UnitPrice> findEffectiveUnitPrice(String itemCode, String supplierCode, LocalDate date);

    void deleteAll();
}
