package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.purchase.Acceptance;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface AcceptanceMapper {

    void insert(Acceptance acceptance);

    Optional<Acceptance> findById(@Param("id") Integer id);

    Optional<Acceptance> findByAcceptanceNumber(@Param("acceptanceNumber") String acceptanceNumber);

    List<Acceptance> findByInspectionNumber(@Param("inspectionNumber") String inspectionNumber);

    List<Acceptance> findByPurchaseOrderNumber(@Param("purchaseOrderNumber") String purchaseOrderNumber);

    Optional<String> findLatestAcceptanceNumber(@Param("prefix") String prefix);

    void deleteAll();
}
