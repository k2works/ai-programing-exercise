package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.quality.DefectMaster;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 欠点マスタマッパー
 */
@Mapper
public interface DefectMasterMapper {

    void insert(DefectMaster defect);

    Optional<DefectMaster> findByDefectCode(@Param("defectCode") String defectCode);

    List<DefectMaster> findAll();

    void deleteAll();
}
