package com.example.production.application.port.out;

import com.example.production.domain.model.purchase.Defect;

import java.util.List;
import java.util.Optional;

/**
 * 欠点マスタリポジトリ
 */
public interface DefectRepository {

    void save(Defect defect);

    Optional<Defect> findById(Integer id);

    Optional<Defect> findByDefectCode(String defectCode);

    List<Defect> findAll();

    void deleteAll();
}
