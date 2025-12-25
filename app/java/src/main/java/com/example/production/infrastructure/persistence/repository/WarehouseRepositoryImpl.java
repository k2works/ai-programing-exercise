package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.WarehouseRepository;
import com.example.production.domain.model.inventory.Warehouse;
import com.example.production.infrastructure.persistence.mapper.WarehouseMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 倉庫リポジトリ実装
 */
@Repository
public class WarehouseRepositoryImpl implements WarehouseRepository {

    private final WarehouseMapper warehouseMapper;

    public WarehouseRepositoryImpl(WarehouseMapper warehouseMapper) {
        this.warehouseMapper = warehouseMapper;
    }

    @Override
    public void save(Warehouse warehouse) {
        warehouseMapper.insert(warehouse);
    }

    @Override
    public Optional<Warehouse> findByWarehouseCode(String warehouseCode) {
        return warehouseMapper.findByWarehouseCode(warehouseCode);
    }

    @Override
    public List<Warehouse> findByDepartmentCode(String departmentCode) {
        return warehouseMapper.findByDepartmentCode(departmentCode);
    }

    @Override
    public void deleteAll() {
        warehouseMapper.deleteAll();
    }
}
