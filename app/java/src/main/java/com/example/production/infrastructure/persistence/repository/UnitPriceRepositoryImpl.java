package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.UnitPriceRepository;
import com.example.production.domain.model.purchase.UnitPrice;
import com.example.production.infrastructure.persistence.mapper.UnitPriceMapper;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.Optional;

/**
 * 単価マスタリポジトリ実装
 */
@Repository
public class UnitPriceRepositoryImpl implements UnitPriceRepository {

    private final UnitPriceMapper unitPriceMapper;

    public UnitPriceRepositoryImpl(UnitPriceMapper unitPriceMapper) {
        this.unitPriceMapper = unitPriceMapper;
    }

    @Override
    public void save(UnitPrice unitPrice) {
        unitPriceMapper.insert(unitPrice);
    }

    @Override
    public Optional<UnitPrice> findEffectiveUnitPrice(String itemCode, String supplierCode, LocalDate date) {
        return unitPriceMapper.findEffectiveUnitPrice(itemCode, supplierCode, date);
    }

    @Override
    public void deleteAll() {
        unitPriceMapper.deleteAll();
    }
}
