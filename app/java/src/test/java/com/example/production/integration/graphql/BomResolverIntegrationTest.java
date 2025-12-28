package com.example.production.integration.graphql;

import com.example.production.domain.model.bom.Bom;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.infrastructure.out.mapper.BomMapper;
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

import java.math.BigDecimal;
import java.time.LocalDate;

import static org.assertj.core.api.Assertions.assertThat;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@Testcontainers
@DisplayName("BOM GraphQL リゾルバ")
class BomResolverIntegrationTest {

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

    @Autowired
    private BomMapper bomMapper;

    private HttpGraphQlTester graphQlTester;

    @BeforeEach
    void setUp() {
        bomMapper.deleteAll();
        itemMapper.deleteAll();
        WebTestClient client = WebTestClient.bindToServer()
            .baseUrl("http://localhost:" + port + "/graphql")
            .build();
        graphQlTester = HttpGraphQlTester.create(client);
    }

    @Nested
    @DisplayName("Query: bomTree")
    class BomTreeQueryTests {

        @Test
        @DisplayName("BOM ツリーを取得できる")
        void shouldGetBomTree() {
            // Arrange: 製品 -> 部品A, 部品B の構造を作成
            insertTestItem("PROD-001", "製品A", ItemCategory.PRODUCT);
            insertTestItem("PART-001", "部品A", ItemCategory.PART);
            insertTestItem("PART-002", "部品B", ItemCategory.PART);

            insertBom("PROD-001", "PART-001", new BigDecimal("2"));
            insertBom("PROD-001", "PART-002", new BigDecimal("3"));

            // Act & Assert
            graphQlTester.document("""
                    query {
                        bomTree(itemCode: "PROD-001") {
                            itemCode
                            itemName
                            level
                            children {
                                itemCode
                                itemName
                                requiredQuantity
                                level
                            }
                        }
                    }
                    """)
                .execute()
                .path("bomTree.itemCode").entity(String.class).isEqualTo("PROD-001")
                .path("bomTree.itemName").entity(String.class).isEqualTo("製品A")
                .path("bomTree.level").entity(Integer.class).isEqualTo(0)
                .path("bomTree.children").entityList(Object.class).hasSize(2);
        }

        @Test
        @DisplayName("多階層 BOM ツリーを取得できる")
        void shouldGetMultiLevelBomTree() {
            // Arrange: 製品 -> 組立品 -> 部品 の3階層構造
            insertTestItem("PROD-001", "製品", ItemCategory.PRODUCT);
            insertTestItem("ASSY-001", "組立品", ItemCategory.INTERMEDIATE);
            insertTestItem("PART-001", "部品", ItemCategory.PART);

            insertBom("PROD-001", "ASSY-001", new BigDecimal("1"));
            insertBom("ASSY-001", "PART-001", new BigDecimal("4"));

            // Act & Assert
            graphQlTester.document("""
                    query {
                        bomTree(itemCode: "PROD-001") {
                            itemCode
                            itemName
                            children {
                                itemCode
                                itemName
                                children {
                                    itemCode
                                    itemName
                                    requiredQuantity
                                }
                            }
                        }
                    }
                    """)
                .execute()
                .path("bomTree.itemCode").entity(String.class).isEqualTo("PROD-001")
                .path("bomTree.children[0].itemCode").entity(String.class).isEqualTo("ASSY-001")
                .path("bomTree.children[0].children[0].itemCode").entity(String.class).isEqualTo("PART-001");
        }

        @Test
        @DisplayName("子品目がない場合は空の children を返す")
        void shouldReturnEmptyChildrenWhenNoChildren() {
            insertTestItem("PART-001", "末端部品", ItemCategory.PART);

            graphQlTester.document("""
                    query {
                        bomTree(itemCode: "PART-001") {
                            itemCode
                            itemName
                            children {
                                itemCode
                            }
                        }
                    }
                    """)
                .execute()
                .path("bomTree.itemCode").entity(String.class).isEqualTo("PART-001")
                .path("bomTree.children").entityList(Object.class).hasSize(0);
        }

        @Test
        @DisplayName("存在しない品目は null を返す")
        void shouldReturnNullForNonExistentItem() {
            graphQlTester.document("""
                    query {
                        bomTree(itemCode: "NOT-EXIST") {
                            itemCode
                        }
                    }
                    """)
                .execute()
                .path("bomTree").valueIsNull();
        }
    }

    @Nested
    @DisplayName("Query: whereUsed")
    class WhereUsedQueryTests {

        @Test
        @DisplayName("使用先を取得できる")
        void shouldGetWhereUsed() {
            // Arrange: 部品が複数の製品で使用される構造
            insertTestItem("PROD-001", "製品A", ItemCategory.PRODUCT);
            insertTestItem("PROD-002", "製品B", ItemCategory.PRODUCT);
            insertTestItem("PART-001", "共通部品", ItemCategory.PART);

            insertBom("PROD-001", "PART-001", new BigDecimal("2"));
            insertBom("PROD-002", "PART-001", new BigDecimal("5"));

            // Act & Assert
            graphQlTester.document("""
                    query {
                        whereUsed(itemCode: "PART-001") {
                            parentItemCode
                            itemName
                            requiredQuantity
                            level
                        }
                    }
                    """)
                .execute()
                .path("whereUsed").entityList(Object.class).hasSize(2);
        }

        @Test
        @DisplayName("使用先がない場合は空リストを返す")
        void shouldReturnEmptyListWhenNotUsed() {
            insertTestItem("PROD-001", "製品（使用されていない）", ItemCategory.PRODUCT);

            graphQlTester.document("""
                    query {
                        whereUsed(itemCode: "PROD-001") {
                            parentItemCode
                        }
                    }
                    """)
                .execute()
                .path("whereUsed").entityList(Object.class).hasSize(0);
        }

        @Test
        @DisplayName("存在しない品目も空リストを返す")
        void shouldReturnEmptyListForNonExistentItem() {
            graphQlTester.document("""
                    query {
                        whereUsed(itemCode: "NOT-EXIST") {
                            parentItemCode
                        }
                    }
                    """)
                .execute()
                .path("whereUsed").entityList(Object.class).hasSize(0);
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

    private void insertBom(String parentCode, String childCode, BigDecimal quantity) {
        bomMapper.insert(Bom.builder()
            .parentItemCode(parentCode)
            .childItemCode(childCode)
            .effectiveFrom(LocalDate.of(2025, 1, 1))
            .requiredQuantity(quantity)
            .build());
    }
}
