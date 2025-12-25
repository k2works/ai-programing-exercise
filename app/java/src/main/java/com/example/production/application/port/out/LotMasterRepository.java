package com.example.production.application.port.out;

import com.example.production.domain.model.quality.LotMaster;

import java.util.List;
import java.util.Optional;

/**
 * ロットマスタリポジトリ
 */
public interface LotMasterRepository {

    void save(LotMaster lot);

    Optional<LotMaster> findByLotNumber(String lotNumber);

    List<LotMaster> findByItemCode(String itemCode);

    void deleteAll();
}
