package com.example.sales.dto;

import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validation;
import jakarta.validation.Validator;
import jakarta.validation.ValidatorFactory;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;

class ProductDtoTest {

    private static Validator validator;

    @BeforeAll
    static void setUp() {
        ValidatorFactory factory = Validation.buildDefaultValidatorFactory();
        validator = factory.getValidator();
    }

    @Test
    void 正常な商品作成リクエストを検証できる() {
        CreateProductRequest request = new CreateProductRequest();
        request.setProductCode("PROD00001");
        request.setFullName("黒毛和牛サーロインステーキ 200g");
        request.setName("サーロイン");
        request.setKanaName("クロゲワギュウサーロイン");
        request.setUnitPrice(5000);
        request.setPrimeCost(3500);
        request.setSupplierCode("SUP00001");

        Set<ConstraintViolation<CreateProductRequest>> violations =
            validator.validate(request);

        assertThat(violations).isEmpty();
    }

    @Test
    void 負の単価を拒否する() {
        CreateProductRequest request = new CreateProductRequest();
        request.setProductCode("PROD00001");
        request.setFullName("黒毛和牛サーロインステーキ 200g");
        request.setName("サーロイン");
        request.setKanaName("クロゲワギュウサーロイン");
        request.setUnitPrice(-100);  // 負の値
        request.setPrimeCost(3500);
        request.setSupplierCode("SUP00001");

        Set<ConstraintViolation<CreateProductRequest>> violations =
            validator.validate(request);

        assertThat(violations).isNotEmpty();
        assertThat(violations).anyMatch(v ->
            v.getPropertyPath().toString().equals("unitPrice"));
    }

    @Test
    void 必須フィールド欠落を拒否する() {
        CreateProductRequest request = new CreateProductRequest();
        request.setProductCode("PROD00001");
        // fullName が欠落
        request.setName("サーロイン");
        request.setKanaName("クロゲワギュウサーロイン");
        request.setUnitPrice(5000);
        request.setPrimeCost(3500);
        request.setSupplierCode("SUP00001");

        Set<ConstraintViolation<CreateProductRequest>> violations =
            validator.validate(request);

        assertThat(violations).isNotEmpty();
        assertThat(violations).anyMatch(v ->
            v.getPropertyPath().toString().equals("fullName"));
    }

    @Test
    void 正常な商品更新リクエストを検証できる() {
        UpdateProductRequest request = new UpdateProductRequest();
        request.setFullName("黒毛和牛サーロインステーキ 250g");
        request.setUnitPrice(5500);

        Set<ConstraintViolation<UpdateProductRequest>> violations =
            validator.validate(request);

        assertThat(violations).isEmpty();
    }

    @Test
    void 空のオブジェクトを許可する() {
        UpdateProductRequest request = new UpdateProductRequest();

        Set<ConstraintViolation<UpdateProductRequest>> violations =
            validator.validate(request);

        assertThat(violations).isEmpty();
    }
}
