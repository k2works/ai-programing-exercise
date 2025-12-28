package com.example.production.integration;

import com.example.production.application.port.out.BomRepository;
import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.UnitRepository;
import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.item.Unit;
import com.example.production.testsetup.TestcontainersConfiguration;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.context.annotation.Import;
import org.springframework.web.client.RestClient;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * BOM API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("BOM API 統合テスト")
class BomApiIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private BomRepository bomRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private UnitRepository unitRepository;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);

        // テストデータをクリーンアップして作成
        bomRepository.deleteAll();
        itemRepository.deleteAll();

        // 単位マスタのセットアップ
        if (!unitRepository.existsByUnitCode("PCS")) {
            unitRepository.save(Unit.builder()
                    .unitCode("PCS").unitSymbol("個").unitName("個数").build());
        }
        if (!unitRepository.existsByUnitCode("KG")) {
            unitRepository.save(Unit.builder()
                    .unitCode("KG").unitSymbol("kg").unitName("キログラム").build());
        }

        // 親品目を作成
        Item parentItem = Item.builder()
                .itemCode("BOM-PARENT")
                .itemName("親製品")
                .itemCategory(ItemCategory.PRODUCT)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .unitCode("PCS")
                .leadTime(7)
                .build();
        itemRepository.save(parentItem);

        // 子品目を作成
        Item childItem1 = Item.builder()
                .itemCode("BOM-CHILD-1")
                .itemName("子部品1")
                .itemCategory(ItemCategory.PART)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .unitCode("PCS")
                .leadTime(3)
                .build();
        itemRepository.save(childItem1);

        Item childItem2 = Item.builder()
                .itemCode("BOM-CHILD-2")
                .itemName("子部品2")
                .itemCategory(ItemCategory.MATERIAL)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .unitCode("KG")
                .leadTime(5)
                .build();
        itemRepository.save(childItem2);

        // 孫品目を作成（多階層BOM用）
        Item grandchildItem = Item.builder()
                .itemCode("BOM-GRANDCHILD")
                .itemName("孫部品")
                .itemCategory(ItemCategory.MATERIAL)
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .unitCode("PCS")
                .leadTime(2)
                .build();
        itemRepository.save(grandchildItem);

        // BOMを作成
        Bom bom1 = Bom.builder()
                .parentItemCode("BOM-PARENT")
                .childItemCode("BOM-CHILD-1")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .baseQuantity(BigDecimal.ONE)
                .requiredQuantity(new BigDecimal("2"))
                .sequence(1)
                .build();
        bomRepository.save(bom1);

        Bom bom2 = Bom.builder()
                .parentItemCode("BOM-PARENT")
                .childItemCode("BOM-CHILD-2")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .baseQuantity(BigDecimal.ONE)
                .requiredQuantity(new BigDecimal("3.5"))
                .sequence(2)
                .build();
        bomRepository.save(bom2);

        Bom bom3 = Bom.builder()
                .parentItemCode("BOM-CHILD-1")
                .childItemCode("BOM-GRANDCHILD")
                .effectiveFrom(LocalDate.of(2025, 1, 1))
                .baseQuantity(BigDecimal.ONE)
                .requiredQuantity(new BigDecimal("4"))
                .sequence(1)
                .build();
        bomRepository.save(bom3);
    }

    @Nested
    @DisplayName("GET /api/bom")
    class GetAllBom {

        @Test
        @DisplayName("すべてのBOMを取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnAllBom() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSizeGreaterThanOrEqualTo(3);
        }
    }

    @Nested
    @DisplayName("GET /api/bom/{itemCode}/children")
    class GetChildren {

        @Test
        @DisplayName("子品目一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnChildren() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-PARENT/children")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(2);
            assertThat(response.get(0).get("parentItemCode")).isEqualTo("BOM-PARENT");
            assertThat(response.get(0).get("childItemCode")).isEqualTo("BOM-CHILD-1");
        }

        @Test
        @DisplayName("子品目がない場合は空配列を返す")
        @SuppressWarnings("unchecked")
        void shouldReturnEmptyArrayWhenNoChildren() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-GRANDCHILD/children")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isEmpty();
        }

        @Test
        @DisplayName("存在しない品目コードでも空配列を返す")
        @SuppressWarnings("unchecked")
        void shouldReturnEmptyArrayForNonExistentItem() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/NON-EXISTENT/children")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isEmpty();
        }
    }

    @Nested
    @DisplayName("GET /api/bom/{itemCode}/parents")
    class GetParents {

        @Test
        @DisplayName("親品目一覧を取得できる（使用先照会）")
        @SuppressWarnings("unchecked")
        void shouldReturnParents() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-CHILD-1/parents")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(1);
            assertThat(response.get(0).get("parentItemCode")).isEqualTo("BOM-PARENT");
            assertThat(response.get(0).get("childItemCode")).isEqualTo("BOM-CHILD-1");
        }

        @Test
        @DisplayName("親品目がない場合は空配列を返す")
        @SuppressWarnings("unchecked")
        void shouldReturnEmptyArrayWhenNoParents() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-PARENT/parents")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isEmpty();
        }

        @Test
        @DisplayName("孫品目から親を辿れる")
        @SuppressWarnings("unchecked")
        void shouldReturnParentOfGrandchild() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-GRANDCHILD/parents")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(1);
            assertThat(response.get(0).get("parentItemCode")).isEqualTo("BOM-CHILD-1");
        }
    }

    @Nested
    @DisplayName("GET /api/bom/{itemCode}/explode")
    class ExplodeBom {

        @Test
        @DisplayName("BOM展開で全階層の部品を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnAllLevelsOfBom() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-PARENT/explode")
                    .retrieve()
                    .body(List.class);

            // BOM-PARENT -> BOM-CHILD-1 (2), BOM-CHILD-2 (3.5)
            // BOM-CHILD-1 -> BOM-GRANDCHILD (4)
            // 合計3件の展開結果
            assertThat(response).isNotNull()
                    .hasSize(3);

            // 階層1: 直下の子品目
            var level1Items = response.stream()
                    .filter(r -> ((Number) r.get("level")).intValue() == 1)
                    .toList();
            assertThat(level1Items).hasSize(2);

            // 階層2: 孫品目
            var level2Items = response.stream()
                    .filter(r -> ((Number) r.get("level")).intValue() == 2)
                    .toList();
            assertThat(level2Items).hasSize(1);
            assertThat(level2Items.get(0).get("childItemCode")).isEqualTo("BOM-GRANDCHILD");
        }

        @Test
        @DisplayName("数量を指定してBOM展開できる")
        @SuppressWarnings("unchecked")
        void shouldExplodeWithQuantity() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-PARENT/explode?quantity=10")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(3);

            // BOM-CHILD-1: 基準数量1に対して必要数量2 -> 10 * 2 = 20
            var child1 = response.stream()
                    .filter(r -> "BOM-CHILD-1".equals(r.get("childItemCode")))
                    .findFirst()
                    .orElseThrow();
            assertThat(((Number) child1.get("totalQuantity")).doubleValue())
                    .isEqualTo(20.0);
        }

        @Test
        @DisplayName("子品目がない品目を展開すると空配列を返す")
        @SuppressWarnings("unchecked")
        void shouldReturnEmptyForLeafItem() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/bom/BOM-GRANDCHILD/explode")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isEmpty();
        }
    }
}
