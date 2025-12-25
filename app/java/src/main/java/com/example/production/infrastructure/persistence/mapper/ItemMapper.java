package com.example.production.infrastructure.persistence.mapper;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Mapper
public interface ItemMapper {
    void insert(Item item);

    Optional<Item> findByItemCode(String itemCode);

    Optional<Item> findByItemCodeAndDate(@Param("itemCode") String itemCode,
                                          @Param("baseDate") LocalDate baseDate);

    List<Item> findAll();

    List<Item> findByCategory(@Param("category") ItemCategory category);

    void update(Item item);

    void deleteByItemCode(String itemCode);

    void deleteAll();
}
