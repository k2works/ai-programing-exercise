package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.ItemRepository;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@DisplayName("品目リポジトリ")
class ItemRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private ItemRepository itemRepository;

    @BeforeEach
    void setUp() {
        itemRepository.deleteAll();
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("品目を登録できる")
        void canRegisterItem() {
            // Arrange
            var item = Item.builder()
                    .itemCode("ITEM001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("製品A")
                    .itemCategory(ItemCategory.PRODUCT)
                    .build();

            // Act
            itemRepository.save(item);

            // Assert
            assertThat(item.getId()).isNotNull();

            var result = itemRepository.findByItemCode("ITEM001");
            assertThat(result).isPresent();
            assertThat(result.get().getItemName()).isEqualTo("製品A");
            assertThat(result.get().getItemCategory()).isEqualTo(ItemCategory.PRODUCT);
        }

        @Test
        @DisplayName("同じ品目コードでも適用開始日が異なれば登録できる")
        void canRegisterSameCodeWithDifferentEffectiveDate() {
            // Arrange
            var item1 = Item.builder()
                    .itemCode("ITEM001")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("製品A（旧）")
                    .itemCategory(ItemCategory.PRODUCT)
                    .build();
            itemRepository.save(item1);

            var item2 = Item.builder()
                    .itemCode("ITEM001")
                    .effectiveFrom(LocalDate.of(2025, 4, 1))
                    .itemName("製品A（新）")
                    .itemCategory(ItemCategory.PRODUCT)
                    .build();

            // Act
            itemRepository.save(item2);

            // Assert
            assertThat(item2.getId()).isNotNull();
            assertThat(item2.getId()).isNotEqualTo(item1.getId());
        }
    }

    @Nested
    @DisplayName("生産管理属性")
    class ProductionAttributes {

        @Test
        @DisplayName("リードタイムと安全在庫を設定できる")
        void canSetLeadTimeAndSafetyStock() {
            // Arrange
            var item = Item.builder()
                    .itemCode("ITEM002")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("製品B")
                    .itemCategory(ItemCategory.PRODUCT)
                    .leadTime(5)
                    .safetyLeadTime(2)
                    .safetyStock(new BigDecimal("100"))
                    .build();

            // Act
            itemRepository.save(item);

            // Assert
            var result = itemRepository.findByItemCode("ITEM002");
            assertThat(result).isPresent();
            assertThat(result.get().getLeadTime()).isEqualTo(5);
            assertThat(result.get().getSafetyLeadTime()).isEqualTo(2);
            assertThat(result.get().getSafetyStock()).isEqualByComparingTo(new BigDecimal("100"));
        }

        @Test
        @DisplayName("ロットサイズを設定できる")
        void canSetLotSize() {
            // Arrange
            var item = Item.builder()
                    .itemCode("ITEM003")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .itemName("製品C")
                    .itemCategory(ItemCategory.PRODUCT)
                    .minLotSize(new BigDecimal("10"))
                    .lotIncrement(new BigDecimal("5"))
                    .maxLotSize(new BigDecimal("1000"))
                    .build();

            // Act
            itemRepository.save(item);

            // Assert
            var result = itemRepository.findByItemCode("ITEM003");
            assertThat(result).isPresent();
            assertThat(result.get().getMinLotSize()).isEqualByComparingTo(new BigDecimal("10"));
            assertThat(result.get().getLotIncrement()).isEqualByComparingTo(new BigDecimal("5"));
            assertThat(result.get().getMaxLotSize()).isEqualByComparingTo(new BigDecimal("1000"));
        }
    }

    @Nested
    @DisplayName("品目区分")
    class ItemCategories {

        @Test
        @DisplayName("全ての品目区分を登録できる")
        void canRegisterAllCategories() {
            var categories = ItemCategory.values();

            for (int i = 0; i < categories.length; i++) {
                var item = Item.builder()
                        .itemCode("CAT-" + String.format("%03d", i))
                        .effectiveFrom(LocalDate.of(2025, 1, 1))
                        .itemName("品目" + categories[i].getDisplayName())
                        .itemCategory(categories[i])
                        .build();

                itemRepository.save(item);

                var result = itemRepository.findByItemCode(item.getItemCode());
                assertThat(result).isPresent();
                assertThat(result.get().getItemCategory()).isEqualTo(categories[i]);
            }
        }
    }
}
