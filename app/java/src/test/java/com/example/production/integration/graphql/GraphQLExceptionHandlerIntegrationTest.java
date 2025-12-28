package com.example.production.integration.graphql;

import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.infrastructure.persistence.mapper.ItemMapper;
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
@DisplayName("GraphQL 例外ハンドラ")
class GraphQLExceptionHandlerIntegrationTest {

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
    @DisplayName("NOT_FOUND エラー")
    class NotFoundErrorTests {

        @Test
        @DisplayName("存在しない品目の更新は NOT_FOUND エラーを返す")
        void shouldReturnNotFoundForNonExistentItemUpdate() {
            graphQlTester.document("""
                    mutation {
                        updateItem(input: {
                            itemCode: "NOT-EXIST"
                            itemName: "更新品目"
                        }) {
                            itemCode
                        }
                    }
                    """)
                .execute()
                .errors()
                .satisfy(errors -> {
                    assertThat(errors).hasSize(1);
                    assertThat(errors.get(0).getMessage()).contains("NOT-EXIST");
                });
        }
    }

    @Nested
    @DisplayName("BAD_REQUEST エラー")
    class BadRequestErrorTests {

        @Test
        @DisplayName("重複する品目コードは BAD_REQUEST エラーを返す")
        void shouldReturnBadRequestForDuplicateItem() {
            // Arrange
            insertTestItem("DUP-001", "既存品目", ItemCategory.PRODUCT);

            // Act & Assert
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
                .satisfy(errors -> {
                    assertThat(errors).hasSize(1);
                    assertThat(errors.get(0).getMessage()).contains("DUP-001");
                });
        }
    }

    @Nested
    @DisplayName("エラーメッセージのフォーマット")
    class ErrorMessageFormatTests {

        @Test
        @DisplayName("エラーメッセージは分かりやすい形式で返される")
        void shouldReturnReadableErrorMessage() {
            graphQlTester.document("""
                    mutation {
                        updateItem(input: {
                            itemCode: "UNKNOWN-ITEM"
                            itemName: "テスト"
                        }) {
                            itemCode
                        }
                    }
                    """)
                .execute()
                .errors()
                .satisfy(errors -> {
                    assertThat(errors).isNotEmpty();
                    // エラーメッセージが空でないことを確認
                    assertThat(errors.get(0).getMessage()).isNotBlank();
                });
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
