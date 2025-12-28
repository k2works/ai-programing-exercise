package com.example.production.integration.graphql;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.infrastructure.out.mapper.ItemMapper;
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

import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("品目 GraphQL リゾルバ")
class ItemResolverIntegrationTest {

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
    private ItemMapper itemMapper;

    private HttpGraphQlTester graphQlTester;

    @BeforeEach
    void setUp() {
        itemMapper.deleteAll();
        WebTestClient client = WebTestClient.bindToServer()
            .baseUrl("http://localhost:" + port + "/graphql")
            .build();
        graphQlTester = HttpGraphQlTester.create(client);
    }

    @Nested
    @DisplayName("Query: item")
    class ItemQueryTests {

        @Test
        @DisplayName("品目コードで品目を取得できる")
        void shouldGetItemByCode() {
            // Arrange
            insertTestItem("PROD-001", "テスト製品", ItemCategory.PRODUCT);

            // Act & Assert
            graphQlTester.document("""
                    query {
                        item(itemCode: "PROD-001") {
                            itemCode
                            itemName
                            category
                        }
                    }
                    """)
                .execute()
                .path("item.itemCode").entity(String.class).isEqualTo("PROD-001")
                .path("item.itemName").entity(String.class).isEqualTo("テスト製品")
                .path("item.category").entity(String.class).isEqualTo("PRODUCT");
        }

        @Test
        @DisplayName("存在しない品目は null を返す")
        void shouldReturnNullForNonExistentItem() {
            graphQlTester.document("""
                    query {
                        item(itemCode: "NOT-EXIST") {
                            itemCode
                        }
                    }
                    """)
                .execute()
                .path("item").valueIsNull();
        }
    }

    @Nested
    @DisplayName("Query: items")
    class ItemsQueryTests {

        @Test
        @DisplayName("品目一覧を取得できる")
        void shouldGetItems() {
            for (int i = 1; i <= 5; i++) {
                insertTestItem("ITEM-" + String.format("%03d", i),
                    "品目" + i, ItemCategory.PRODUCT);
            }

            graphQlTester.document("""
                    query {
                        items(page: 0, size: 10) {
                            edges {
                                node {
                                    itemCode
                                    itemName
                                }
                            }
                            pageInfo {
                                totalElements
                            }
                        }
                    }
                    """)
                .execute()
                .path("items.edges").entityList(Object.class).hasSize(5)
                .path("items.pageInfo.totalElements").entity(Integer.class).isEqualTo(5);
        }

        @Test
        @DisplayName("カテゴリでフィルタリングできる")
        void shouldFilterByCategory() {
            insertTestItem("PROD-001", "製品1", ItemCategory.PRODUCT);
            insertTestItem("PART-001", "部品1", ItemCategory.PART);
            insertTestItem("PROD-002", "製品2", ItemCategory.PRODUCT);

            graphQlTester.document("""
                    query {
                        items(category: PRODUCT, page: 0, size: 10) {
                            edges {
                                node {
                                    itemCode
                                }
                            }
                            pageInfo {
                                totalElements
                            }
                        }
                    }
                    """)
                .execute()
                .path("items.pageInfo.totalElements").entity(Integer.class).isEqualTo(2);
        }
    }

    @Nested
    @DisplayName("Mutation: createItem")
    class CreateItemMutationTests {

        @Test
        @DisplayName("品目を登録できる")
        void shouldCreateItem() {
            graphQlTester.document("""
                    mutation {
                        createItem(input: {
                            itemCode: "NEW-001"
                            itemName: "新規品目"
                            category: PRODUCT
                            leadTime: 5
                        }) {
                            itemCode
                            itemName
                            category
                            leadTime
                        }
                    }
                    """)
                .execute()
                .path("createItem.itemCode").entity(String.class).isEqualTo("NEW-001")
                .path("createItem.itemName").entity(String.class).isEqualTo("新規品目")
                .path("createItem.category").entity(String.class).isEqualTo("PRODUCT")
                .path("createItem.leadTime").entity(Integer.class).isEqualTo(5);

            // DB に保存されていることを確認
            var saved = itemMapper.findByItemCode("NEW-001");
            assertThat(saved).isPresent();
            assertThat(saved.get().getItemName()).isEqualTo("新規品目");
        }

        @Test
        @DisplayName("重複する品目コードはエラーになる")
        void shouldFailForDuplicateCode() {
            insertTestItem("DUP-001", "既存品目", ItemCategory.PRODUCT);

            graphQlTester.document("""
                    mutation {
                        createItem(input: {
                            itemCode: "DUP-001"
                            itemName: "重複品目"
                            category: PRODUCT
                        }) {
                            itemCode
                        }
                    }
                    """)
                .execute()
                .errors()
                .satisfy(errors -> assertThat(errors).hasSize(1));
        }
    }

    @Nested
    @DisplayName("Mutation: updateItem")
    class UpdateItemMutationTests {

        @Test
        @DisplayName("品目を更新できる")
        void shouldUpdateItem() {
            insertTestItem("UPD-001", "更新前", ItemCategory.PRODUCT);

            graphQlTester.document("""
                    mutation {
                        updateItem(input: {
                            itemCode: "UPD-001"
                            itemName: "更新後"
                            leadTime: 10
                        }) {
                            itemCode
                            itemName
                            leadTime
                        }
                    }
                    """)
                .execute()
                .path("updateItem.itemCode").entity(String.class).isEqualTo("UPD-001")
                .path("updateItem.itemName").entity(String.class).isEqualTo("更新後")
                .path("updateItem.leadTime").entity(Integer.class).isEqualTo(10);
        }
    }

    @Nested
    @DisplayName("Mutation: deleteItem")
    class DeleteItemMutationTests {

        @Test
        @DisplayName("品目を削除できる")
        void shouldDeleteItem() {
            insertTestItem("DEL-001", "削除対象", ItemCategory.PRODUCT);

            graphQlTester.document("""
                    mutation {
                        deleteItem(itemCode: "DEL-001")
                    }
                    """)
                .execute()
                .path("deleteItem").entity(Boolean.class).isEqualTo(true);

            // DB から削除されていることを確認
            var deleted = itemMapper.findByItemCode("DEL-001");
            assertThat(deleted).isEmpty();
        }

        @Test
        @DisplayName("存在しない品目の削除は false を返す")
        void shouldReturnFalseForNonExistentItem() {
            graphQlTester.document("""
                    mutation {
                        deleteItem(itemCode: "NOT-EXIST")
                    }
                    """)
                .execute()
                .path("deleteItem").entity(Boolean.class).isEqualTo(false);
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
}
