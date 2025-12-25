package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.inventory.Warehouse;
import org.apache.ibatis.annotations.Mapper;

import java.util.List;
import java.util.Optional;

@Mapper
public interface WarehouseMapper {
    void insert(Warehouse warehouse);
    Optional<Warehouse> findByWarehouseCode(String warehouseCode);
    List<Warehouse> findByDepartmentCode(String departmentCode);
    void deleteAll();
}
