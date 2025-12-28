package com.example.production.application.port.out;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;

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
     * 品目区分で品目を取得する
     */
    List<Item> findByCategory(ItemCategory category);

    /**
     * 複数の品目コードで品目を取得する（バッチ取得）
     */
    List<Item> findByItemCodes(List<String> itemCodes);

    /**
     * 品目を更新する
     */
    void update(Item item);

    /**
     * 品目コードで品目を削除する
     */
    void deleteByItemCode(String itemCode);

    /**
     * すべての品目を削除する
     */
    void deleteAll();
}
