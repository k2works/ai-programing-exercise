package com.example.sales.controller;

import com.example.sales.AbstractDatabaseTest;
import com.example.sales.dto.CreateProductRequest;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.http.MediaType;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.transaction.annotation.Transactional;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@AutoConfigureMockMvc
@Transactional
class ProductControllerTest extends AbstractDatabaseTest {

    @Autowired
    private MockMvc mockMvc;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private JdbcTemplate jdbcTemplate;

    @BeforeEach
    void setUp() {
        // 商品分類マスタのテストデータを挿入
        jdbcTemplate.execute("INSERT INTO 商品分類マスタ " +
                "(商品分類コード, 商品分類名, 商品分類階層, 商品分類パス, 最下層区分, 作成者名, 更新者名) " +
                "VALUES ('CAT001', 'テスト分類', 1, '/CAT001', 1, 'SYSTEM', 'SYSTEM') " +
                "ON CONFLICT (商品分類コード) DO NOTHING");
    }

    @Test
    void 商品を作成できる() throws Exception {
        CreateProductRequest request = new CreateProductRequest();
        request.setProductCode("TEST00001");
        request.setFullName("テスト商品");
        request.setName("テスト");
        request.setKanaName("テストショウヒン");
        request.setUnitPrice(1000);
        request.setPrimeCost(700);
        request.setSupplierCode("SUP00001");

        mockMvc.perform(post("/api/products")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated())
            .andExpect(jsonPath("$.productCode").value("TEST00001"))
            .andExpect(jsonPath("$.fullName").value("テスト商品"));
    }

    @Test
    void すべての商品を取得できる() throws Exception {
        mockMvc.perform(get("/api/products"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$").isArray());
    }

    @Test
    void IDで商品を取得できる() throws Exception {
        // 事前にテストデータを投入
        CreateProductRequest request = new CreateProductRequest();
        request.setProductCode("TEST00002");
        request.setFullName("テスト商品2");
        request.setName("テスト2");
        request.setKanaName("テストショウヒン2");
        request.setUnitPrice(1000);
        request.setPrimeCost(700);
        request.setSupplierCode("SUP00001");

        mockMvc.perform(post("/api/products")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isCreated());

        mockMvc.perform(get("/api/products/TEST00002"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.productCode").value("TEST00002"));
    }

    @Test
    void 存在しない商品を取得すると404エラー() throws Exception {
        mockMvc.perform(get("/api/products/NONEXISTENT"))
            .andExpect(status().isNotFound());
    }

    @Test
    void バリデーションエラーで400エラー() throws Exception {
        CreateProductRequest request = new CreateProductRequest();
        // 必須フィールドを設定しない

        mockMvc.perform(post("/api/products")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isBadRequest());
    }
}
