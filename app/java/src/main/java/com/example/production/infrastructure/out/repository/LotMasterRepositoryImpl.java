package com.example.production.infrastructure.out.repository;

import com.example.production.application.port.out.LotMasterRepository;
import com.example.production.domain.model.quality.LotMaster;
import com.example.production.infrastructure.out.mapper.LotMasterMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

/**
 * ロットマスタリポジトリ実装
 */
@Repository
@RequiredArgsConstructor
public class LotMasterRepositoryImpl implements LotMasterRepository {

    private final LotMasterMapper lotMasterMapper;

    @Override
    public void save(LotMaster lot) {
        lotMasterMapper.insert(lot);
    }

    @Override
    public Optional<LotMaster> findByLotNumber(String lotNumber) {
        return lotMasterMapper.findByLotNumber(lotNumber);
    }

    @Override
    public List<LotMaster> findByItemCode(String itemCode) {
        return lotMasterMapper.findByItemCode(itemCode);
    }

    @Override
    public void deleteAll() {
        lotMasterMapper.deleteAll();
    }
}
