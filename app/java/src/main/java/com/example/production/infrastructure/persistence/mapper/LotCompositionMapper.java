package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.quality.LotComposition;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * ロット構成マッパー
 */
@Mapper
public interface LotCompositionMapper {

    void insert(LotComposition composition);

    List<LotComposition> findByParentLotNumber(@Param("parentLotNumber") String parentLotNumber);

    List<LotComposition> findByChildLotNumber(@Param("childLotNumber") String childLotNumber);

    void deleteAll();
}
