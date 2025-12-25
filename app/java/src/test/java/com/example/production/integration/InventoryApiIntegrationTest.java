package com.example.production.integration;

import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.LocationRepository;
import com.example.production.application.port.out.StockRepository;
import com.example.production.application.port.out.UnitRepository;
import com.example.production.domain.model.inventory.Stock;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.item.Unit;
import com.example.production.domain.model.location.Location;
import com.example.production.domain.model.location.LocationType;
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
 * 在庫 API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("在庫 API 統合テスト")
class InventoryApiIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private StockRepository stockRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private LocationRepository locationRepository;

    @Autowired
    private UnitRepository unitRepository;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);

        // テストデータをクリーンアップ
        stockRepository.deleteAll();

        // 単位マスタのセットアップ
        if (!unitRepository.existsByUnitCode("PCS")) {
            unitRepository.save(Unit.builder()
                    .unitCode("PCS").unitSymbol("個").unitName("個数").build());
        }

        // 場所マスタのセットアップ
        if (locationRepository.findByLocationCode("WH-TEST").isEmpty()) {
            locationRepository.save(Location.builder()
                    .locationCode("WH-TEST")
                    .locationName("テスト倉庫")
                    .locationType(LocationType.WAREHOUSE)
                    .build());
        }

        // 品目のセットアップ
        if (itemRepository.findByItemCode("TEST-ITEM-INV").isEmpty()) {
            itemRepository.save(Item.builder()
                    .itemCode("TEST-ITEM-INV")
                    .itemName("テスト品目（在庫用）")
                    .itemCategory(ItemCategory.MATERIAL)
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .unitCode("PCS")
                    .leadTime(5)
                    .build());
        }

        // 在庫データのセットアップ
        Stock stock = Stock.builder()
                .locationCode("WH-TEST")
                .itemCode("TEST-ITEM-INV")
                .stockQuantity(new BigDecimal("100"))
                .passedQuantity(new BigDecimal("80"))
                .defectiveQuantity(new BigDecimal("10"))
                .uninspectedQuantity(new BigDecimal("10"))
                .build();
        stockRepository.save(stock);
    }

    @Nested
    @DisplayName("GET /api/inventory")
    class GetInventory {

        @Test
        @DisplayName("在庫一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnAllInventory() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/inventory")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSizeGreaterThanOrEqualTo(1);
        }

        @Test
        @DisplayName("品目コードでフィルタリングできる")
        @SuppressWarnings("unchecked")
        void shouldFilterByItemCode() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/inventory?itemCode=TEST-ITEM-INV")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(1);
            assertThat(response.get(0).get("itemCode")).isEqualTo("TEST-ITEM-INV");
        }

        @Test
        @DisplayName("場所コードでフィルタリングできる")
        @SuppressWarnings("unchecked")
        void shouldFilterByLocationCode() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/inventory?locationCode=WH-TEST")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(1);
            assertThat(response.get(0).get("locationCode")).isEqualTo("WH-TEST");
        }
    }

    @Nested
    @DisplayName("GET /api/inventory/location/{locationCode}")
    class GetStocksByLocation {

        @Test
        @DisplayName("場所別在庫一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnStocksByLocation() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/inventory/location/WH-TEST")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(1);
        }

        @Test
        @DisplayName("存在しない場所は空配列を返す")
        @SuppressWarnings("unchecked")
        void shouldReturnEmptyArrayForNonExistentLocation() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/inventory/location/NON-EXISTENT")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .isEmpty();
        }
    }

    @Nested
    @DisplayName("GET /api/inventory/item/{itemCode}")
    class GetStocksByItem {

        @Test
        @DisplayName("品目別在庫一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnStocksByItem() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/inventory/item/TEST-ITEM-INV")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull()
                    .hasSize(1);
        }
    }

    @Nested
    @DisplayName("GET /api/inventory/{locationCode}/{itemCode}")
    class GetStock {

        @Test
        @DisplayName("特定の在庫を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnStock() {
            Map<String, Object> response = restClient.get()
                    .uri("/api/inventory/WH-TEST/TEST-ITEM-INV")
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("locationCode")).isEqualTo("WH-TEST");
            assertThat(response.get("itemCode")).isEqualTo("TEST-ITEM-INV");
            assertThat(((Number) response.get("stockQuantity")).intValue()).isEqualTo(100);
        }
    }
}
