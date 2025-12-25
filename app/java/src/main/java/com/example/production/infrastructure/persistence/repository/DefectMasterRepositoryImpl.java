package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.DefectMasterRepository;
import com.example.production.domain.model.quality.DefectMaster;
import com.example.production.infrastructure.persistence.mapper.DefectMasterMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 欠点マスタリポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class DefectMasterRepositoryImpl implements DefectMasterRepository {

    private final DefectMasterMapper defectMasterMapper;

    @Override
    public void save(DefectMaster defect) {
        defectMasterMapper.insert(defect);
    }

    @Override
    public Optional<DefectMaster> findByDefectCode(String defectCode) {
        return defectMasterMapper.findByDefectCode(defectCode);
    }

    @Override
    public List<DefectMaster> findAll() {
        return defectMasterMapper.findAll();
    }

    @Override
    public void deleteAll() {
        defectMasterMapper.deleteAll();
    }
}
