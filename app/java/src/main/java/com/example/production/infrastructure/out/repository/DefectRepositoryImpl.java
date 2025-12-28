package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.DefectRepository;
import com.example.production.domain.model.purchase.Defect;
import com.example.production.infrastructure.out.mapper.DefectMapper;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * 欠点マスタリポジトリ実装
 */
@Repository
public class DefectRepositoryImpl implements DefectRepository {

    private final DefectMapper defectMapper;

    public DefectRepositoryImpl(DefectMapper defectMapper) {
        this.defectMapper = defectMapper;
    }

    @Override
    public void save(Defect defect) {
        defectMapper.insert(defect);
    }

    @Override
    public Optional<Defect> findById(Integer id) {
        return defectMapper.findById(id);
    }

    @Override
    public Optional<Defect> findByDefectCode(String defectCode) {
        return defectMapper.findByDefectCode(defectCode);
    }

    @Override
    public List<Defect> findAll() {
        return defectMapper.findAll();
    }

    @Override
    public void deleteAll() {
        defectMapper.deleteAll();
    }
}
