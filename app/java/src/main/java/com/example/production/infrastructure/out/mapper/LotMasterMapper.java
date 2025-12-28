package com.example.production.infrastructure.out.mapper;

import com.example.production.domain.model.quality.LotMaster;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * ロットマスタマッパー
 */
@Mapper
public interface LotMasterMapper {

    void insert(LotMaster lot);

    Optional<LotMaster> findByLotNumber(@Param("lotNumber") String lotNumber);

    List<LotMaster> findByItemCode(@Param("itemCode") String itemCode);

    void deleteAll();
}
