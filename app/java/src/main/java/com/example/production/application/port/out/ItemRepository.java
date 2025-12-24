package com.example.production.application.port.out;

import com.example.production.domain.model.item.Item;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

/**
 * 品目リポジトリ（Output Port）
 * ドメイン層がデータアクセスに依存しないためのインターフェース
 */
public interface ItemRepository {

    /**
     * 品目を保存する
     */
    void save(Item item);

    /**
     * 品目コードで品目を検索する（最新の適用開始日）
     */
    Optional<Item> findByItemCode(String itemCode);

    /**
     * 品目コードと基準日で品目を検索する
     */
    Optional<Item> findByItemCodeAndDate(String itemCode, LocalDate baseDate);

    /**
     * すべての品目を取得する
     */
    List<Item> findAll();

    /**
     * 品目を更新する
     */
    void update(Item item);

    /**
     * すべての品目を削除する
     */
    void deleteAll();
}
