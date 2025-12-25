package com.example.production.application.port.out;

import com.example.production.domain.model.quality.DefectMaster;

import java.util.List;
import java.util.Optional;

/**
 * 欠点マスタリポジトリ
 */
public interface DefectMasterRepository {

    void save(DefectMaster defect);

    Optional<DefectMaster> findByDefectCode(String defectCode);

    List<DefectMaster> findAll();

    void deleteAll();
}
