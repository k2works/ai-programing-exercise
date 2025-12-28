package com.example.production.application.service;

import com.example.production.application.port.in.ItemUseCase;
import com.example.production.application.port.in.command.CreateItemCommand;
import com.example.production.application.port.in.command.UpdateItemCommand;
import com.example.production.application.port.out.ItemRepository;
import com.example.production.domain.exception.DuplicateItemException;
import com.example.production.domain.exception.ItemNotFoundException;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDate;
import java.util.List;

/**
 * 品目アプリケーションサービス
 */
@Service
@RequiredArgsConstructor
@Transactional
public class ItemService implements ItemUseCase {

    private final ItemRepository itemRepository;

    @Override
    public Item createItem(CreateItemCommand command) {
        // 重複チェック
        itemRepository.findByItemCode(command.getItemCode())
                .ifPresent(existing -> {
                    throw new DuplicateItemException(command.getItemCode());
                });

        Item item = Item.builder()
                .itemCode(command.getItemCode())
                .effectiveFrom(LocalDate.now())
                .itemName(command.getItemName())
                .itemCategory(command.getCategory())
                .unitCode(command.getUnitCode())
                .leadTime(command.getLeadTime() != null ? command.getLeadTime() : 0)
                .safetyLeadTime(command.getSafetyLeadTime() != null ? command.getSafetyLeadTime() : 0)
                .safetyStock(command.getSafetyStock())
                .yieldRate(command.getYieldRate())
                .minLotSize(command.getMinLotSize())
                .lotIncrement(command.getLotIncrement())
                .maxLotSize(command.getMaxLotSize())
                .shelfLife(command.getShelfLife())
                .build();

        itemRepository.save(item);
        return item;
    }

    @Override
    public Item updateItem(UpdateItemCommand command) {
        Item item = itemRepository.findByItemCode(command.getItemCode())
                .orElseThrow(() -> new ItemNotFoundException(command.getItemCode()));

        // 更新処理（null でない場合のみ更新）
        applyUpdates(item, command);

        itemRepository.update(item);
        return item;
    }

    private void applyUpdates(Item item, UpdateItemCommand command) {
        if (command.getItemName() != null) item.setItemName(command.getItemName());
        if (command.getCategory() != null) item.setItemCategory(command.getCategory());
        if (command.getLeadTime() != null) item.setLeadTime(command.getLeadTime());
        if (command.getSafetyLeadTime() != null) item.setSafetyLeadTime(command.getSafetyLeadTime());
        if (command.getSafetyStock() != null) item.setSafetyStock(command.getSafetyStock());
        if (command.getYieldRate() != null) item.setYieldRate(command.getYieldRate());
        if (command.getMinLotSize() != null) item.setMinLotSize(command.getMinLotSize());
        if (command.getLotIncrement() != null) item.setLotIncrement(command.getLotIncrement());
        if (command.getMaxLotSize() != null) item.setMaxLotSize(command.getMaxLotSize());
        if (command.getShelfLife() != null) item.setShelfLife(command.getShelfLife());
    }

    @Override
    @Transactional(readOnly = true)
    public List<Item> getAllItems() {
        return itemRepository.findAll();
    }

    @Override
    @Transactional(readOnly = true)
    public Item getItemByCode(String itemCode) {
        return itemRepository.findByItemCode(itemCode)
                .orElseThrow(() -> new ItemNotFoundException(itemCode));
    }

    @Override
    @Transactional(readOnly = true)
    public List<Item> getItemsByCategory(ItemCategory category) {
        return itemRepository.findByCategory(category);
    }

    @Override
    @Transactional(readOnly = true)
    public List<Item> getItemsByCodes(List<String> itemCodes) {
        return itemRepository.findByItemCodes(itemCodes);
    }

    @Override
    public void deleteItem(String itemCode) {
        itemRepository.findByItemCode(itemCode)
                .orElseThrow(() -> new ItemNotFoundException(itemCode));

        itemRepository.deleteByItemCode(itemCode);
    }
}
