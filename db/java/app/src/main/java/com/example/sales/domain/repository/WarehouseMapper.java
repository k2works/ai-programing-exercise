package com.example.sales.domain.repository;

import com.example.sales.domain.model.Warehouse;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Optional;

/**
 * 倉庫マスタのMapperインターフェース
 */
@Mapper
public interface WarehouseMapper {
    /**
     * 倉庫を登録する
     * @param warehouse 倉庫
     * @return 登録件数
     */
    int insert(Warehouse warehouse);

    /**
     * 倉庫を更新する
     * @param warehouse 倉庫
     * @return 更新件数
     */
    int update(Warehouse warehouse);

    /**
     * 倉庫を削除する
     * @param warehouseCode 倉庫コード
     * @return 削除件数
     */
    int delete(@Param("warehouseCode") String warehouseCode);

    /**
     * 倉庫コードで倉庫を取得する
     * @param warehouseCode 倉庫コード
     * @return 倉庫
     */
    Optional<Warehouse> findById(@Param("warehouseCode") String warehouseCode);

    /**
     * すべての倉庫を取得する
     * @return 倉庫のリスト
     */
    List<Warehouse> findAll();
}
