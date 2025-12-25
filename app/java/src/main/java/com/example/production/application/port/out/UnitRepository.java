package com.example.production.application.port.out;

import com.example.production.domain.model.item.Unit;

import java.util.List;
import java.util.Optional;

/**
 * 単位リポジトリ
 */
public interface UnitRepository {

    /**
     * 単位コードで検索する
     *
     * @param unitCode 単位コード
     * @return 単位
     */
    Optional<Unit> findByUnitCode(String unitCode);

    /**
     * すべての単位を取得する
     *
     * @return 単位リスト
     */
    List<Unit> findAll();

    /**
     * 単位を保存する
     *
     * @param unit 単位
     */
    void save(Unit unit);

    /**
     * 単位が存在するか確認する
     *
     * @param unitCode 単位コード
     * @return 存在する場合 true
     */
    boolean existsByUnitCode(String unitCode);

    /**
     * すべての単位を削除する
     */
    void deleteAll();
}
