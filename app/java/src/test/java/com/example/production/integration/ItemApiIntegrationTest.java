package com.example.production.integration;

import com.example.production.application.port.out.ItemRepository;
import com.example.production.application.port.out.UnitRepository;
import com.example.production.domain.model.item.Item;
import com.example.production.domain.model.item.ItemCategory;
import com.example.production.domain.model.item.Unit;
import com.example.production.testsetup.BaseIntegrationTest;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.webmvc.test.autoconfigure.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.Map;

import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.hasSize;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * 品目 API インテグレーションテスト
 */
@AutoConfigureMockMvc
@DisplayName("品目 API")
class ItemApiIntegrationTest extends BaseIntegrationTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private UnitRepository unitRepository;

    private final ObjectMapper objectMapper = new ObjectMapper()
            .registerModule(new JavaTimeModule());

    private Item testItem;

    @BeforeEach
    void setUp() {
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

        testItem = Item.builder()
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
        void shouldReturnAllItems() throws Exception {
            mockMvc.perform(get("/api/items"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$").isArray())
                    .andExpect(jsonPath("$", hasSize(greaterThanOrEqualTo(1))));
        }

        @Test
        @DisplayName("カテゴリでフィルタリングできる")
        void shouldFilterByCategory() throws Exception {
            mockMvc.perform(get("/api/items")
                            .param("category", "PRODUCT"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$").isArray())
                    .andExpect(jsonPath("$[0].category").value("PRODUCT"));
        }
    }

    @Nested
    @DisplayName("GET /api/items/category/{category}")
    class GetItemsByCategory {

        @Test
        @DisplayName("カテゴリ別に品目を取得できる")
        void shouldReturnItemsByCategory() throws Exception {
            mockMvc.perform(get("/api/items/category/PRODUCT"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$").isArray())
                    .andExpect(jsonPath("$[0].category").value("PRODUCT"));
        }

        @Test
        @DisplayName("無効なカテゴリで400エラーを返す")
        void shouldReturn400ForInvalidCategory() throws Exception {
            mockMvc.perform(get("/api/items/category/INVALID"))
                    .andExpect(status().isBadRequest());
        }
    }

    @Nested
    @DisplayName("GET /api/items/{itemCode}")
    class GetItemByCode {

        @Test
        @DisplayName("品目コードで品目を取得できる")
        void shouldReturnItemByCode() throws Exception {
            mockMvc.perform(get("/api/items/TEST-001"))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.itemCode").value("TEST-001"))
                    .andExpect(jsonPath("$.itemName").value("テスト品目"))
                    .andExpect(jsonPath("$.category").value("PRODUCT"));
        }

        @Test
        @DisplayName("存在しない品目コードで404エラーを返す")
        void shouldReturn404ForNonExistentItem() throws Exception {
            mockMvc.perform(get("/api/items/NON-EXISTENT"))
                    .andExpect(status().isNotFound())
                    .andExpect(jsonPath("$.title").value("品目が見つかりません"));
        }
    }

    @Nested
    @DisplayName("POST /api/items")
    class CreateItem {

        @Test
        @DisplayName("新規品目を登録できる")
        void shouldCreateNewItem() throws Exception {
            var request = Map.of(
                    "itemCode", "NEW-001",
                    "itemName", "新規品目",
                    "category", "MATERIAL",
                    "unitCode", "KG",
                    "leadTime", 14
            );

            mockMvc.perform(post("/api/items")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isCreated())
                    .andExpect(jsonPath("$.itemCode").value("NEW-001"))
                    .andExpect(jsonPath("$.itemName").value("新規品目"))
                    .andExpect(jsonPath("$.category").value("MATERIAL"));
        }

        @Test
        @DisplayName("重複する品目コードで409エラーを返す")
        void shouldReturn409ForDuplicateItemCode() throws Exception {
            var request = Map.of(
                    "itemCode", "TEST-001",
                    "itemName", "重複品目",
                    "category", "PRODUCT"
            );

            mockMvc.perform(post("/api/items")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isConflict())
                    .andExpect(jsonPath("$.title").value("品目コード重複"));
        }

        @Test
        @DisplayName("必須項目が欠けている場合400エラーを返す")
        void shouldReturn400ForMissingRequiredFields() throws Exception {
            var request = Map.of(
                    "itemCode", "NEW-002"
                    // itemName と category が欠けている
            );

            mockMvc.perform(post("/api/items")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isBadRequest());
        }
    }

    @Nested
    @DisplayName("PUT /api/items/{itemCode}")
    class UpdateItem {

        @Test
        @DisplayName("品目を更新できる")
        void shouldUpdateItem() throws Exception {
            var request = Map.of(
                    "itemName", "更新後の品目名",
                    "leadTime", 10
            );

            mockMvc.perform(put("/api/items/TEST-001")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isOk())
                    .andExpect(jsonPath("$.itemCode").value("TEST-001"))
                    .andExpect(jsonPath("$.itemName").value("更新後の品目名"))
                    .andExpect(jsonPath("$.leadTime").value(10));
        }

        @Test
        @DisplayName("存在しない品目を更新しようとすると404エラーを返す")
        void shouldReturn404ForNonExistentItem() throws Exception {
            var request = Map.of(
                    "itemName", "更新品目"
            );

            mockMvc.perform(put("/api/items/NON-EXISTENT")
                            .contentType(MediaType.APPLICATION_JSON)
                            .content(objectMapper.writeValueAsString(request)))
                    .andExpect(status().isNotFound());
        }
    }

    @Nested
    @DisplayName("DELETE /api/items/{itemCode}")
    class DeleteItem {

        @Test
        @DisplayName("品目を削除できる")
        void shouldDeleteItem() throws Exception {
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

            mockMvc.perform(delete("/api/items/TEST-002"))
                    .andExpect(status().isNoContent());

            // 削除されていることを確認
            mockMvc.perform(get("/api/items/TEST-002"))
                    .andExpect(status().isNotFound());
        }

        @Test
        @DisplayName("存在しない品目を削除しようとすると404エラーを返す")
        void shouldReturn404ForNonExistentItem() throws Exception {
            mockMvc.perform(delete("/api/items/NON-EXISTENT"))
                    .andExpect(status().isNotFound());
        }
    }
}
