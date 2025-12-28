package com.example.production.integration.rest;

import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.PurchaseOrderDetailRepository;
import com.example.production.application.port.out.PurchaseOrderRepository;
import com.example.production.application.port.out.SupplierRepository;
import com.example.production.application.port.out.UnitRepository;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.item.Unit;
import com.example.production.domain.model.supplier.Supplier;
import com.example.production.domain.model.supplier.SupplierType;
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

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * 発注 API 統合テスト
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Import(TestcontainersConfiguration.class)
@DisplayName("発注 API 統合テスト")
class PurchaseOrderApiIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private PurchaseOrderRepository purchaseOrderRepository;

    @Autowired
    private PurchaseOrderDetailRepository purchaseOrderDetailRepository;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private SupplierRepository supplierRepository;

    @Autowired
    private UnitRepository unitRepository;

    private RestClient restClient;

    @BeforeEach
    void setUp() {
        restClient = RestClient.create("http://localhost:" + port);

        // テストデータをクリーンアップ（明細を先に削除）
        purchaseOrderDetailRepository.deleteAll();
        purchaseOrderRepository.deleteAll();

        // 単位マスタのセットアップ
        if (!unitRepository.existsByUnitCode("PCS")) {
            unitRepository.save(Unit.builder()
                    .unitCode("PCS").unitSymbol("個").unitName("個数").build());
        }

        // 品目のセットアップ
        if (itemRepository.findByItemCode("TEST-ITEM-PO").isEmpty()) {
            itemRepository.save(Item.builder()
                    .itemCode("TEST-ITEM-PO")
                    .itemName("テスト品目（発注用）")
                    .itemCategory(ItemCategory.MATERIAL)
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .unitCode("PCS")
                    .leadTime(5)
                    .build());
        }

        // 取引先のセットアップ
        if (supplierRepository.findBySupplierCode("TEST-SUPPLIER").isEmpty()) {
            supplierRepository.save(Supplier.builder()
                    .supplierCode("TEST-SUPPLIER")
                    .supplierName("テスト取引先")
                    .supplierType(SupplierType.VENDOR)
                    .effectiveFrom(LocalDate.of(2025, 1, 1))
                    .build());
        }
    }

    @Nested
    @DisplayName("GET /api/purchase-orders")
    class GetAllOrders {

        @Test
        @DisplayName("発注一覧を取得できる")
        @SuppressWarnings("unchecked")
        void shouldReturnAllOrders() {
            List<Map<String, Object>> response = restClient.get()
                    .uri("/api/purchase-orders")
                    .retrieve()
                    .body(List.class);

            assertThat(response).isNotNull();
        }
    }

    @Nested
    @DisplayName("POST /api/purchase-orders")
    class CreateOrder {

        @Test
        @DisplayName("発注を登録できる")
        @SuppressWarnings("unchecked")
        void shouldCreateOrder() {
            String request = """
                {
                    "supplierCode": "TEST-SUPPLIER",
                    "details": [
                        {
                            "itemCode": "TEST-ITEM-PO",
                            "orderQuantity": 100,
                            "unitPrice": 1000,
                            "deliveryDate": "2025-02-01"
                        }
                    ]
                }
                """;

            Map<String, Object> response = restClient.post()
                    .uri("/api/purchase-orders")
                    .contentType(MediaType.APPLICATION_JSON)
                    .body(request)
                    .retrieve()
                    .body(Map.class);

            assertThat(response).isNotNull();
            assertThat(response.get("purchaseOrderNumber")).isNotNull();
            assertThat(response.get("supplierCode")).isEqualTo("TEST-SUPPLIER");
            assertThat(response.get("status")).isEqualTo("CREATING");
        }
    }

    @Nested
    @DisplayName("GET /api/purchase-orders/{orderNumber}")
    class GetOrder {

        @Test
        @DisplayName("存在しない発注番号は404を返す")
        void shouldReturn404ForNonExistentOrder() {
            assertThatThrownBy(() -> restClient.get()
                    .uri("/api/purchase-orders/NON-EXISTENT")
                    .retrieve()
                    .body(Map.class))
                    .isInstanceOf(HttpClientErrorException.class)
                    .satisfies(ex -> {
                        HttpClientErrorException httpEx = (HttpClientErrorException) ex;
                        assertThat(httpEx.getStatusCode()).isEqualTo(HttpStatus.NOT_FOUND);
                    });
        }
    }
}
