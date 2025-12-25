package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.purchase.Inspection;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface InspectionMapper {

    void insert(Inspection inspection);

    Optional<Inspection> findById(@Param("id") Integer id);

    Optional<Inspection> findByInspectionNumber(@Param("inspectionNumber") String inspectionNumber);

    List<Inspection> findByReceivingNumber(@Param("receivingNumber") String receivingNumber);

    Optional<String> findLatestInspectionNumber(@Param("prefix") String prefix);

    void deleteAll();
}
