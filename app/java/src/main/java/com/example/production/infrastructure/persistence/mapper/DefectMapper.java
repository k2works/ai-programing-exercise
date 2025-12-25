package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.purchase.Defect;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

@Mapper
public interface DefectMapper {

    void insert(Defect defect);

    Optional<Defect> findById(@Param("id") Integer id);

    Optional<Defect> findByDefectCode(@Param("defectCode") String defectCode);

    List<Defect> findAll();

    void deleteAll();
}
