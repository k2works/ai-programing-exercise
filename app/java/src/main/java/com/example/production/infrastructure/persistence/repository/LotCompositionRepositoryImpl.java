package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.LotCompositionRepository;
import com.example.production.domain.model.quality.LotComposition;
import com.example.production.infrastructure.persistence.mapper.LotCompositionMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * ロット構成リポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class LotCompositionRepositoryImpl implements LotCompositionRepository {

    private final LotCompositionMapper lotCompositionMapper;

    @Override
    public void save(LotComposition composition) {
        lotCompositionMapper.insert(composition);
    }

    @Override
    public List<LotComposition> findByParentLotNumber(String parentLotNumber) {
        return lotCompositionMapper.findByParentLotNumber(parentLotNumber);
    }

    @Override
    public List<LotComposition> findByChildLotNumber(String childLotNumber) {
        return lotCompositionMapper.findByChildLotNumber(childLotNumber);
    }

    @Override
    public void deleteAll() {
        lotCompositionMapper.deleteAll();
    }
}
