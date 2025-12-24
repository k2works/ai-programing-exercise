package com.example.production.infrastructure.persistence.repository;

import com.example.production.application.port.out.BomRepository;
import com.example.production.application.port.out.ItemRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.testsetup.BaseIntegrationTest;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.*;

@DisplayName("BOM（部品構成表）リポジトリ")
class BomRepositoryTest extends BaseIntegrationTest {

    @Autowired
    private BomRepository bomRepository;

    @Autowired
    private ItemRepository itemRepository;

    @BeforeEach
    void setUp() {
        bomRepository.deleteAll();
        itemRepository.deleteAll();
    }

    @Nested
    @DisplayName("登録")
    class Registration {

        @Test
        @DisplayName("親品目と子品目の関係を登録できる")
        void canRegisterParentChildRelation() {
            // Arrange: 親品目と子品目を作成
            var parent = createItem("PRODUCT-X", "製品X", ItemCategory.PRODUCT);
            itemRepository.save(parent);

            var child = createItem("PART-A", "部品A", ItemCategory.PART);
            itemRepository.save(child);

            // Act: BOMを登録
            var bom = Bom.builder()
                    .parentItemCode("PRODUCT-X")
                    .childItemCode("PART-A")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .baseQuantity(BigDecimal.ONE)
                    .requiredQuantity(new BigDecimal("2"))
                    .build();
            bomRepository.save(bom);

            // Assert
            var children = bomRepository.findByParentItemCode("PRODUCT-X");
            assertThat(children).hasSize(1);
            assertThat(children.get(0).getChildItemCode()).isEqualTo("PART-A");
            assertThat(children.get(0).getRequiredQuantity()).isEqualByComparingTo(new BigDecimal("2"));
        }
    }

    @Nested
    @DisplayName("展開")
    class Explosion {

        @Test
        @DisplayName("1階層の部品展開ができる")
        void canExplodeSingleLevel() {
            // Arrange
            var product = createItem("PRODUCT-X", "製品X", ItemCategory.PRODUCT);
            itemRepository.save(product);

            var partA = createItem("PART-A", "部品A", ItemCategory.PART);
            itemRepository.save(partA);

            var partB = createItem("PART-B", "部品B", ItemCategory.PART);
            itemRepository.save(partB);

            // BOMを登録
            bomRepository.save(createBom("PRODUCT-X", "PART-A", 2));
            bomRepository.save(createBom("PRODUCT-X", "PART-B", 3));

            // Act
            var children = bomRepository.findByParentItemCode("PRODUCT-X");

            // Assert
            assertThat(children).hasSize(2);
            assertThat(children.stream().map(Bom::getChildItemCode).toList())
                    .containsExactlyInAnyOrder("PART-A", "PART-B");
        }

        @Test
        @DisplayName("不良率を考慮した必要数量を計算できる")
        void canCalculateWithDefectRate() {
            // Arrange
            var product = createItem("PRODUCT-Y", "製品Y", ItemCategory.PRODUCT);
            itemRepository.save(product);

            var part = createItem("PART-C", "部品C", ItemCategory.PART);
            itemRepository.save(part);

            var bom = Bom.builder()
                    .parentItemCode("PRODUCT-Y")
                    .childItemCode("PART-C")
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .baseQuantity(BigDecimal.ONE)
                    .requiredQuantity(new BigDecimal("10"))
                    .defectRate(new BigDecimal("5")) // 5%不良
                    .build();
            bomRepository.save(bom);

            // Act
            var children = bomRepository.findByParentItemCode("PRODUCT-Y");

            // Assert: 10 × (1 + 0.05) = 10.5
            var result = children.get(0);
            var actualQuantity = result.getRequiredQuantity()
                    .multiply(BigDecimal.ONE.add(result.getDefectRate().divide(new BigDecimal("100"))));
            assertThat(actualQuantity).isEqualByComparingTo(new BigDecimal("10.5"));
        }
    }

    private Item createItem(String itemCode, String itemName, ItemCategory category) {
        return Item.builder()
                .itemCode(itemCode)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .itemName(itemName)
                .itemCategory(category)
                .build();
    }

    private Bom createBom(String parentCode, String childCode, int quantity) {
        return Bom.builder()
                .parentItemCode(parentCode)
                .childItemCode(childCode)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .baseQuantity(BigDecimal.ONE)
                .requiredQuantity(BigDecimal.valueOf(quantity))
                .build();
    }
}
