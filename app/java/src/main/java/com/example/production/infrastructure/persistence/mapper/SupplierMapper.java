package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Mapper
public interface SupplierMapper {
    void insert(Supplier supplier);
    Optional<Supplier> findBySupplierCode(String supplierCode);
    Optional<Supplier> findBySupplierCodeAndDate(@Param("supplierCode") String supplierCode,
                                                  @Param("baseDate") LocalDate baseDate);
    List<Supplier> findBySupplierType(SupplierType supplierType);
    List<Supplier> findAll();
    void deleteAll();
}
