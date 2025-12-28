package com.example.production.integration.graphql;

import com.example.production.domain.model.plan.Order;
import com.example.production.domain.model.plan.OrderType;
import com.example.production.domain.model.plan.PlanStatus;
import com.example.production.domain.model.plan.Requirement;
import com.example.production.infrastructure.persistence.mapper.OrderMapper;
import com.example.production.infrastructure.persistence.mapper.RequirementMapper;
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

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("MRP GraphQL リゾルバ")
class MrpResolverIntegrationTest {

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
    private OrderMapper orderMapper;

    @Autowired
    private RequirementMapper requirementMapper;

    private HttpGraphQlTester graphQlTester;

    @BeforeEach
    void setUp() {
        requirementMapper.deleteAll();
        orderMapper.deleteAll();

        WebTestClient client = WebTestClient.bindToServer()
            .baseUrl("http://localhost:" + port + "/graphql")
            .build();
        graphQlTester = HttpGraphQlTester.create(client);
    }

    @Nested
    @DisplayName("Mutation: executeMrp")
    class ExecuteMrpMutationTests {

        @Test
        @DisplayName("MRP を実行できる")
        void shouldExecuteMrp() {
            // Arrange
            LocalDate startDate = LocalDate.of(2025, 1, 1);
            LocalDate endDate = LocalDate.of(2025, 1, 31);

            // テストデータを登録
            insertTestOrder("MO-001", "ITEM-001", OrderType.MANUFACTURING,
                new BigDecimal("100"), LocalDate.of(2025, 1, 15));
            insertTestOrder("PO-001", "ITEM-002", OrderType.PURCHASE,
                new BigDecimal("50"), LocalDate.of(2025, 1, 20));

            // Act & Assert
            graphQlTester.document("""
                    mutation {
                        executeMrp(input: {
                            startDate: "2025-01-01"
                            endDate: "2025-01-31"
                        }) {
                            executionId
                            periodStart
                            periodEnd
                            plannedOrders {
                                itemCode
                                quantity
                                orderType
                            }
                            statistics {
                                totalItemsProcessed
                                purchaseOrderCount
                                productionOrderCount
                            }
                        }
                    }
                    """)
                .execute()
                .path("executeMrp.executionId").hasValue()
                .path("executeMrp.periodStart").entity(String.class).isEqualTo("2025-01-01")
                .path("executeMrp.periodEnd").entity(String.class).isEqualTo("2025-01-31")
                .path("executeMrp.plannedOrders").entityList(Object.class).hasSize(2)
                .path("executeMrp.statistics.totalItemsProcessed").entity(Integer.class).isEqualTo(2)
                .path("executeMrp.statistics.purchaseOrderCount").entity(Integer.class).isEqualTo(1)
                .path("executeMrp.statistics.productionOrderCount").entity(Integer.class).isEqualTo(1);
        }

        @Test
        @DisplayName("期間外のオーダは含まれない")
        void shouldExcludeOrdersOutsidePeriod() {
            // Arrange
            insertTestOrder("MO-001", "ITEM-001", OrderType.MANUFACTURING,
                new BigDecimal("100"), LocalDate.of(2025, 2, 15)); // 期間外

            // Act & Assert
            graphQlTester.document("""
                    mutation {
                        executeMrp(input: {
                            startDate: "2025-01-01"
                            endDate: "2025-01-31"
                        }) {
                            plannedOrders {
                                itemCode
                            }
                            statistics {
                                totalItemsProcessed
                            }
                        }
                    }
                    """)
                .execute()
                .path("executeMrp.plannedOrders").entityList(Object.class).hasSize(0)
                .path("executeMrp.statistics.totalItemsProcessed").entity(Integer.class).isEqualTo(0);
        }

        @Test
        @DisplayName("不足品目を検出できる")
        void shouldDetectShortageItems() {
            // Arrange
            LocalDate dueDate = LocalDate.of(2025, 1, 15);
            Order order = insertTestOrder("MO-001", "ITEM-001", OrderType.MANUFACTURING,
                new BigDecimal("100"), dueDate);

            // 不足がある所要を登録
            insertTestRequirement("REQ-001", order.getId(), "PART-001", dueDate,
                new BigDecimal("50"), new BigDecimal("0"), new BigDecimal("50"));

            // Act & Assert
            graphQlTester.document("""
                    mutation {
                        executeMrp(input: {
                            startDate: "2025-01-01"
                            endDate: "2025-01-31"
                        }) {
                            shortageItems {
                                itemCode
                                shortageQuantity
                            }
                            statistics {
                                shortageItemCount
                            }
                        }
                    }
                    """)
                .execute()
                .path("executeMrp.shortageItems").entityList(Object.class).hasSize(1)
                .path("executeMrp.shortageItems[0].itemCode").entity(String.class).isEqualTo("PART-001")
                .path("executeMrp.statistics.shortageItemCount").entity(Integer.class).isEqualTo(1);
        }
    }

    private Order insertTestOrder(String orderNumber, String itemCode, OrderType orderType,
                                  BigDecimal quantity, LocalDate dueDate) {
        Order order = Order.builder()
            .orderNumber(orderNumber)
            .orderType(orderType)
            .itemCode(itemCode)
            .startDate(dueDate.minusDays(7))
            .dueDate(dueDate)
            .planQuantity(quantity)
            .locationCode("WH-001")
            .status(PlanStatus.CONFIRMED)
            .build();
        orderMapper.insert(order);
        return order;
    }

    private void insertTestRequirement(String reqNumber, Integer orderId, String itemCode,
                                       LocalDate dueDate, BigDecimal required,
                                       BigDecimal allocated, BigDecimal shortage) {
        Requirement requirement = Requirement.builder()
            .requirementNumber(reqNumber)
            .orderId(orderId)
            .itemCode(itemCode)
            .dueDate(dueDate)
            .requiredQuantity(required)
            .allocatedQuantity(allocated)
            .shortageQuantity(shortage)
            .locationCode("WH-001")
            .build();
        requirementMapper.insert(requirement);
    }
}
