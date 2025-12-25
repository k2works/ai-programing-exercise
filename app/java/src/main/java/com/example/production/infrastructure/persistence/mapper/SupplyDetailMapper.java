package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.subcontract.SupplyDetail;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface SupplyDetailMapper {

    void insert(SupplyDetail detail);

    Optional<SupplyDetail> findById(@Param("id") Integer id);

    List<SupplyDetail> findBySupplyNumber(@Param("supplyNumber") String supplyNumber);

    void deleteAll();
}
