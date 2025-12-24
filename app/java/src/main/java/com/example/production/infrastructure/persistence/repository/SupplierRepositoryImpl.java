package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.SupplierRepository;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
import com.example.production.infrastructure.persistence.mapper.SupplierMapper;
import org.springframework.stereotype.Repository;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public class SupplierRepositoryImpl implements SupplierRepository {

    private final SupplierMapper supplierMapper;

    public SupplierRepositoryImpl(SupplierMapper supplierMapper) {
        this.supplierMapper = supplierMapper;
    }

    @Override
    public void save(Supplier supplier) {
        supplierMapper.insert(supplier);
    }

    @Override
    public Optional<Supplier> findBySupplierCode(String supplierCode) {
        return supplierMapper.findBySupplierCode(supplierCode);
    }

    @Override
    public Optional<Supplier> findBySupplierCodeAndDate(String supplierCode, LocalDate baseDate) {
        return supplierMapper.findBySupplierCodeAndDate(supplierCode, baseDate);
    }

    @Override
    public List<Supplier> findBySupplierType(SupplierType supplierType) {
        return supplierMapper.findBySupplierType(supplierType);
    }

    @Override
    public List<Supplier> findAll() {
        return supplierMapper.findAll();
    }

    @Override
    public void deleteAll() {
        supplierMapper.deleteAll();
    }
}
