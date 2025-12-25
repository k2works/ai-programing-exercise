package com.example.production.application.port.out;

import com.example.production.domain.model.quality.LotComposition;

import java.util.List;

/**
 * ロット構成リポジトリ
 */
public interface LotCompositionRepository {

    void save(LotComposition composition);

    List<LotComposition> findByParentLotNumber(String parentLotNumber);

    List<LotComposition> findByChildLotNumber(String childLotNumber);

    void deleteAll();
}
