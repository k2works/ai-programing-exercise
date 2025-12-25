package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.item.Unit;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.util.List;

/**
 * 単位マッパー
 */
@Mapper
public interface UnitMapper {

    /**
     * 単位コードで検索する
     *
     * @param unitCode 単位コード
     * @return 単位
     */
    Unit findByUnitCode(@Param("unitCode") String unitCode);

    /**
     * すべての単位を取得する
     *
     * @return 単位リスト
     */
    List<Unit> findAll();

    /**
     * 単位を挿入する
     *
     * @param unit 単位
     */
    void insert(Unit unit);

    /**
     * 単位が存在するか確認する
     *
     * @param unitCode 単位コード
     * @return 存在する場合 true
     */
    boolean existsByUnitCode(@Param("unitCode") String unitCode);

    /**
     * すべての単位を削除する
     */
    void deleteAll();
}
