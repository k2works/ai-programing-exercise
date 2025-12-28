package com.example.production.integration.graphql;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.purchase.PurchaseOrder;
import com.example.production.domain.model.purchase.PurchaseOrderDetail;
import com.example.production.domain.model.purchase.PurchaseOrderStatus;
import com.example.production.infrastructure.persistence.mapper.ItemMapper;
import com.example.production.infrastructure.persistence.mapper.PurchaseOrderDetailMapper;
import com.example.production.infrastructure.persistence.mapper.PurchaseOrderMapper;
import org.junit.jupiter.api.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.graphql.test.tester.HttpGraphQlTester;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.testcontainers.containers.PostgreSQLContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("発注 GraphQL リゾルバ")
class PurchaseOrderResolverIntegrationTest {

    @Container
    static PostgreSQLContainer<?> postgres = new PostgreSQLContainer<>("postgres:16")
        .withDatabaseName("production_test")
        .withUsername("test")
        .withPassword("test");

    @DynamicPropertySource
    static void configureProperties(DynamicPropertyRegistry registry) {
        registry.add("spring.datasource.url", postgres::getJdbcUrl);
        registry.add("spring.datasource.username", postgres::getUsername);
        registry.add("spring.datasource.password", postgres::getPassword);
        registry.add("seed.enabled", () -> "false");
    }

    @LocalServerPort
    private int port;

    @Autowired
    private PurchaseOrderMapper purchaseOrderMapper;

    @Autowired
    private PurchaseOrderDetailMapper purchaseOrderDetailMapper;

    @Autowired
    private ItemMapper itemMapper;

    private HttpGraphQlTester graphQlTester;

    @BeforeEach
    void setUp() {
        purchaseOrderDetailMapper.deleteAll();
        purchaseOrderMapper.deleteAll();
        itemMapper.deleteAll();

        WebTestClient client = WebTestClient.bindToServer()
            .baseUrl("http://localhost:" + port + "/graphql")
            .build();
        graphQlTester = HttpGraphQlTester.create(client);
    }

    @Nested
    @DisplayName("Query: purchaseOrder")
    class PurchaseOrderQueryTests {

        @Test
        @DisplayName("発注番号で発注を取得できる")
        void shouldGetPurchaseOrderByNumber() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);
            insertTestPurchaseOrder("PO-001", "SUP-001");

            // Act & Assert
            graphQlTester.document("""
                    query {
                        purchaseOrder(purchaseOrderNumber: "PO-001") {
                            purchaseOrderNumber
                            supplierCode
                            status
                            totalOrderAmount
                        }
                    }
                    """)
                .execute()
                .path("purchaseOrder.purchaseOrderNumber").entity(String.class).isEqualTo("PO-001")
                .path("purchaseOrder.supplierCode").entity(String.class).isEqualTo("SUP-001")
                .path("purchaseOrder.status").entity(String.class).isEqualTo("CREATING");
        }

        @Test
        @DisplayName("発注明細も取得できる")
        void shouldGetPurchaseOrderWithDetails() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);
            insertTestPurchaseOrderWithDetail("PO-002", "SUP-001", "ITEM-001");

            // Act & Assert
            graphQlTester.document("""
                    query {
                        purchaseOrder(purchaseOrderNumber: "PO-002") {
                            purchaseOrderNumber
                            details {
                                lineNumber
                                itemCode
                                orderQuantity
                            }
                        }
                    }
                    """)
                .execute()
                .path("purchaseOrder.purchaseOrderNumber").entity(String.class).isEqualTo("PO-002")
                .path("purchaseOrder.details").entityList(Object.class).hasSize(1)
                .path("purchaseOrder.details[0].itemCode").entity(String.class).isEqualTo("ITEM-001");
        }
    }

    @Nested
    @DisplayName("Query: purchaseOrders")
    class PurchaseOrdersQueryTests {

        @Test
        @DisplayName("発注一覧を取得できる")
        void shouldGetPurchaseOrders() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);
            insertTestPurchaseOrder("PO-001", "SUP-001");
            insertTestPurchaseOrder("PO-002", "SUP-002");
            insertTestPurchaseOrder("PO-003", "SUP-001");

            // Act & Assert
            graphQlTester.document("""
                    query {
                        purchaseOrders(page: 0, size: 10) {
                            edges {
                                node {
                                    purchaseOrderNumber
                                    supplierCode
                                }
                            }
                            pageInfo {
                                totalElements
                            }
                        }
                    }
                    """)
                .execute()
                .path("purchaseOrders.edges").entityList(Object.class).hasSize(3)
                .path("purchaseOrders.pageInfo.totalElements").entity(Integer.class).isEqualTo(3);
        }

        @Test
        @DisplayName("ステータスでフィルタリングできる")
        void shouldFilterByStatus() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);
            insertTestPurchaseOrderWithStatus("PO-001", "SUP-001", PurchaseOrderStatus.CREATING);
            insertTestPurchaseOrderWithStatus("PO-002", "SUP-001", PurchaseOrderStatus.ORDERED);
            insertTestPurchaseOrderWithStatus("PO-003", "SUP-001", PurchaseOrderStatus.CREATING);

            // Act & Assert
            graphQlTester.document("""
                    query {
                        purchaseOrders(status: ORDERED, page: 0, size: 10) {
                            pageInfo {
                                totalElements
                            }
                        }
                    }
                    """)
                .execute()
                .path("purchaseOrders.pageInfo.totalElements").entity(Integer.class).isEqualTo(1);
        }
    }

    @Nested
    @DisplayName("Mutation: createPurchaseOrder")
    class CreatePurchaseOrderMutationTests {

        @Test
        @DisplayName("発注を登録できる")
        void shouldCreatePurchaseOrder() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);

            // Act & Assert
            graphQlTester.document("""
                    mutation {
                        createPurchaseOrder(input: {
                            supplierCode: "SUP-001"
                            ordererCode: "EMP-001"
                            departmentCode: "DEPT-001"
                            remarks: "テスト発注"
                            details: [{
                                itemCode: "ITEM-001"
                                orderQuantity: 100
                                unitPrice: 500
                                deliveryDate: "2025-02-01"
                            }]
                        }) {
                            purchaseOrderNumber
                            supplierCode
                            status
                            details {
                                itemCode
                                orderQuantity
                            }
                        }
                    }
                    """)
                .execute()
                .path("createPurchaseOrder.supplierCode").entity(String.class).isEqualTo("SUP-001")
                .path("createPurchaseOrder.status").entity(String.class).isEqualTo("CREATING")
                .path("createPurchaseOrder.details").entityList(Object.class).hasSize(1);
        }
    }

    @Nested
    @DisplayName("Mutation: confirmPurchaseOrder")
    class ConfirmPurchaseOrderMutationTests {

        @Test
        @DisplayName("発注を確定できる")
        void shouldConfirmPurchaseOrder() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);
            insertTestPurchaseOrder("PO-001", "SUP-001");

            // Act & Assert
            graphQlTester.document("""
                    mutation {
                        confirmPurchaseOrder(purchaseOrderNumber: "PO-001") {
                            purchaseOrderNumber
                            status
                        }
                    }
                    """)
                .execute()
                .path("confirmPurchaseOrder.purchaseOrderNumber").entity(String.class).isEqualTo("PO-001")
                .path("confirmPurchaseOrder.status").entity(String.class).isEqualTo("ORDERED");
        }
    }

    @Nested
    @DisplayName("Mutation: cancelPurchaseOrder")
    class CancelPurchaseOrderMutationTests {

        @Test
        @DisplayName("発注を取消できる")
        void shouldCancelPurchaseOrder() {
            // Arrange
            insertTestItem("ITEM-001", "テスト品目", ItemCategory.PART);
            insertTestPurchaseOrder("PO-001", "SUP-001");

            // Act & Assert
            graphQlTester.document("""
                    mutation {
                        cancelPurchaseOrder(purchaseOrderNumber: "PO-001")
                    }
                    """)
                .execute()
                .path("cancelPurchaseOrder").entity(Boolean.class).isEqualTo(true);

            // DB でステータスが取消になっていることを確認
            var order = purchaseOrderMapper.findByPurchaseOrderNumber("PO-001");
            assertThat(order).isPresent();
            assertThat(order.get().getStatus()).isEqualTo(PurchaseOrderStatus.CANCELLED);
        }
    }

    private void insertTestItem(String code, String name, ItemCategory category) {
        itemMapper.insert(Item.builder()
            .itemCode(code)
            .effectiveFrom(LocalDate.of(2025, 1, 1))
            .itemName(name)
            .itemCategory(category)
            .build());
    }

    private void insertTestPurchaseOrder(String orderNumber, String supplierCode) {
        insertTestPurchaseOrderWithStatus(orderNumber, supplierCode, PurchaseOrderStatus.CREATING);
    }

    private void insertTestPurchaseOrderWithStatus(String orderNumber, String supplierCode, PurchaseOrderStatus status) {
        purchaseOrderMapper.insert(PurchaseOrder.builder()
            .purchaseOrderNumber(orderNumber)
            .orderDate(LocalDate.now())
            .supplierCode(supplierCode)
            .status(status)
            .build());
    }

    private void insertTestPurchaseOrderWithDetail(String orderNumber, String supplierCode, String itemCode) {
        purchaseOrderMapper.insert(PurchaseOrder.builder()
            .purchaseOrderNumber(orderNumber)
            .orderDate(LocalDate.now())
            .supplierCode(supplierCode)
            .status(PurchaseOrderStatus.CREATING)
            .build());

        purchaseOrderDetailMapper.insert(PurchaseOrderDetail.builder()
            .purchaseOrderNumber(orderNumber)
            .lineNumber(1)
            .itemCode(itemCode)
            .orderQuantity(new BigDecimal("10"))
            .orderUnitPrice(new BigDecimal("100"))
            .orderAmount(new BigDecimal("1000"))
            .expectedReceivingDate(LocalDate.now().plusDays(7))
            .build());
    }
}
