package com.example.production.integration.rest;

import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.UnitRepository;
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
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestClient;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 品目 API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("品目 API 統合テスト")
class ItemApiIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private UnitRepository unitRepository;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);

        // テストデータをクリーンアップして作成
        itemRepository.findByItemCode("TEST-001").ifPresent(item ->
                itemRepository.deleteByItemCode("TEST-001"));
        itemRepository.findByItemCode("TEST-002").ifPresent(item ->
                itemRepository.deleteByItemCode("TEST-002"));
        itemRepository.findByItemCode("NEW-001").ifPresent(item ->
                itemRepository.deleteByItemCode("NEW-001"));

        // 単位マスタのセットアップ
        if (!unitRepository.existsByUnitCode("PCS")) {
            unitRepository.save(Unit.builder()
                    .unitCode("PCS").unitSymbol("個").unitName("個数").build());
        }
        if (!unitRepository.existsByUnitCode("KG")) {
            unitRepository.save(Unit.builder()
                    .unitCode("KG").unitSymbol("kg").unitName("キログラム").build());
        }

        Item testItem = Item.builder()
                .itemCode("TEST-001")
                .itemName("テスト品目")
                .itemCategory(ItemCategory.PRODUCT)
                .effectiveFrom(LocalDate.now())
                .unitCode("PCS")
                .leadTime(7)
                .safetyLeadTime(0)
                .safetyStock(BigDecimal.valueOf(100))
                .yieldRate(BigDecimal.valueOf(100))
                .minLotSize(BigDecimal.ONE)
                .lotIncrement(BigDecimal.ONE)
                .build();
        itemRepository.save(testItem);
    }

    @Nested
    @DisplayName("GET /api/items")
    class GetAllItems {

        @Test
        @DisplayName("すべての品目を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnAllItems() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/items")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSizeGreaterThanOrEqualTo(1);
        }

        @Test
        @DisplayName("カテゴリでフィルタリングできる")
        @SuppressWarnings("unchecked")
        void shouldFilterByCategory() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/items?category=PRODUCT")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isNotEmpty();
            assertThat(response.get(0).get("category")).isEqualTo("PRODUCT");
        }
    }

    @Nested
    @DisplayName("GET /api/items/category/{category}")
    class GetItemsByCategory {

        @Test
        @DisplayName("カテゴリ別に品目を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnItemsByCategory() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/items/category/PRODUCT")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isNotEmpty();
            assertThat(response.get(0).get("category")).isEqualTo("PRODUCT");
        }

        @Test
        @DisplayName("無効なカテゴリで400エラーを返す")
        void shouldReturn400ForInvalidCategory() {
            assertThatThrownBy(() ->
                    restClient.get()
                            .uri("/api/items/category/INVALID")
                            .retrieve()
                            .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
                    });
        }
    }

    @Nested
    @DisplayName("GET /api/items/{itemCode}")
    class GetItemByCode {

        @Test
        @DisplayName("品目コードで品目を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnItemByCode() {
            Map<String, Object> response = restClient.get()
                    .uri("/api/items/TEST-001")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull()
                    .containsEntry("itemCode", "TEST-001")
                    .containsEntry("itemName", "テスト品目")
                    .containsEntry("category", "PRODUCT");
        }

        @Test
        @DisplayName("存在しない品目コードで404エラーを返す")
        void shouldReturn404ForNonExistentItem() {
            assertThatThrownBy(() ->
                    restClient.get()
                            .uri("/api/items/NON-EXISTENT")
                            .retrieve()
                            .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
                    });
        }
    }

    @Nested
    @DisplayName("POST /api/items")
    class CreateItem {

        @Test
        @DisplayName("新規品目を登録できる")
        @SuppressWarnings("unchecked")
        void shouldCreateNewItem() {
            Map<String, Object> request = Map.of(
                    "itemCode", "NEW-001",
                    "itemName", "新規品目",
                    "category", "MATERIAL",
                    "unitCode", "KG",
                    "leadTime", 14
            );

            Map<String, Object> response = restClient.post()
                    .uri("/api/items")
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(request)
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull()
                    .containsEntry("itemCode", "NEW-001")
                    .containsEntry("itemName", "新規品目")
                    .containsEntry("category", "MATERIAL");
        }

        @Test
        @DisplayName("重複する品目コードで409エラーを返す")
        void shouldReturn409ForDuplicateItemCode() {
            Map<String, Object> request = Map.of(
                    "itemCode", "TEST-001",
                    "itemName", "重複品目",
                    "category", "PRODUCT"
            );

            assertThatThrownBy(() ->
                    restClient.post()
                            .uri("/api/items")
                            .contentType(MediaType.APPLICATION_JSON)
                            .body(request)
                            .retrieve()
                            .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.CONFLICT);
                    });
        }

        @Test
        @DisplayName("必須項目が欠けている場合400エラーを返す")
        void shouldReturn400ForMissingRequiredFields() {
            Map<String, Object> request = Map.of(
                    "itemCode", "NEW-002"
                    // itemName と category が欠けている
            );

            assertThatThrownBy(() ->
                    restClient.post()
                            .uri("/api/items")
                            .contentType(MediaType.APPLICATION_JSON)
                            .body(request)
                            .retrieve()
                            .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.BAD_REQUEST);
                    });
        }
    }

    @Nested
    @DisplayName("PUT /api/items/{itemCode}")
    class UpdateItem {

        @Test
        @DisplayName("品目を更新できる")
        @SuppressWarnings("unchecked")
        void shouldUpdateItem() {
            Map<String, Object> request = Map.of(
                    "itemName", "更新後の品目名",
                    "leadTime", 10
            );

            Map<String, Object> response = restClient.put()
                    .uri("/api/items/TEST-001")
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(request)
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull()
                    .containsEntry("itemCode", "TEST-001")
                    .containsEntry("itemName", "更新後の品目名")
                    .containsEntry("leadTime", 10);
        }

        @Test
        @DisplayName("存在しない品目を更新しようとすると404エラーを返す")
        void shouldReturn404ForNonExistentItem() {
            Map<String, Object> request = Map.of(
                    "itemName", "更新品目"
            );

            assertThatThrownBy(() ->
                    restClient.put()
                            .uri("/api/items/NON-EXISTENT")
                            .contentType(MediaType.APPLICATION_JSON)
                            .body(request)
                            .retrieve()
                            .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
                    });
        }
    }

    @Nested
    @DisplayName("DELETE /api/items/{itemCode}")
    class DeleteItem {

        @Test
        @DisplayName("品目を削除できる")
        void shouldDeleteItem() {
            // テスト用の品目を作成
            Item itemToDelete = Item.builder()
                    .itemCode("TEST-002")
                    .itemName("削除用品目")
                    .itemCategory(ItemCategory.PART)
                    .effectiveFrom(LocalDate.now())
                    .unitCode("PCS")
                    .leadTime(5)
                    .safetyLeadTime(0)
                    .build();
            itemRepository.save(itemToDelete);

            restClient.delete()
                    .uri("/api/items/TEST-002")
                    .retrieve()
                    .toBodilessEntity();

            // 削除されていることを確認
            assertThatThrownBy(() ->
                    restClient.get()
                            .uri("/api/items/TEST-002")
                            .retrieve()
                            .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
                    });
        }

        @Test
        @DisplayName("存在しない品目を削除しようとすると404エラーを返す")
        void shouldReturn404ForNonExistentItem() {
            assertThatThrownBy(() ->
                    restClient.delete()
                            .uri("/api/items/NON-EXISTENT")
                            .retrieve()
                            .toBodilessEntity())
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
                    });
        }
    }
}
