package com.example.production.application.port.in;

import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;

import java.util.List;

/**
 * 品目ユースケース（Input Port）
 */
public interface ItemUseCase {

    /**
     * 品目を登録する
     */
    Item createItem(CreateItemCommand command);

    /**
     * 品目を更新する
     */
    Item updateItem(UpdateItemCommand command);

    /**
     * すべての品目を取得する
     */
    List<Item> getAllItems();

    /**
     * 品目コードで品目を取得する
     */
    Item getItemByCode(String itemCode);

    /**
     * 品目区分で品目を取得する
     */
    List<Item> getItemsByCategory(ItemCategory category);

    /**
     * 複数の品目コードで品目を取得する（バッチ取得）
     */
    List<Item> getItemsByCodes(List<String> itemCodes);

    /**
     * 品目を削除する
     */
    void deleteItem(String itemCode);
}
