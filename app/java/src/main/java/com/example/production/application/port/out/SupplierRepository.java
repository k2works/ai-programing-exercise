package com.example.production.application.port.out;

import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 取引先リポジトリ（Output Port）
 */
public interface SupplierRepository {

    void save(Supplier supplier);

    Optional<Supplier> findBySupplierCode(String supplierCode);

    Optional<Supplier> findBySupplierCodeAndDate(String supplierCode, LocalDate baseDate);

    List<Supplier> findBySupplierType(SupplierType supplierType);

    List<Supplier> findAll();

    void deleteAll();
}
