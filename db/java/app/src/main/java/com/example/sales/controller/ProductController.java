package com.example.sales.controller;

import com.example.sales.dto.*;
import com.example.sales.service.ProductService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@RequestMapping("/api/products")
@RequiredArgsConstructor
@Tag(name = "Product", description = "商品管理API")
public class ProductController {

    private final ProductService productService;

    /**
     * 商品を作成
     */
    @PostMapping
    @Operation(summary = "商品作成", description = "新しい商品を作成します")
    public ResponseEntity<ProductResponse> createProduct(
            @Valid @RequestBody CreateProductRequest request) {
        ProductResponse response = productService.createProduct(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }

    /**
     * すべての商品を取得
     */
    @GetMapping
    @Operation(summary = "商品一覧取得", description = "すべての商品を取得します")
    public ResponseEntity<List<ProductResponse>> getAllProducts() {
        List<ProductResponse> products = productService.getAllProducts();
        return ResponseEntity.ok(products);
    }

    /**
     * ページング対応の商品一覧取得
     */
    @GetMapping("/page")
    @Operation(summary = "商品一覧取得（ページング）",
               description = "ページング対応で商品を取得します")
    public ResponseEntity<PageResponse<ProductResponse>> getProducts(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        PageResponse<ProductResponse> response =
            productService.getProducts(page, size);
        return ResponseEntity.ok(response);
    }

    /**
     * IDで商品を取得
     */
    @GetMapping("/{productCode}")
    @Operation(summary = "商品取得", description = "指定された商品コードの商品を取得します")
    public ResponseEntity<ProductResponse> getProductById(
            @PathVariable String productCode) {
        ProductResponse response = productService.getProductById(productCode);
        return ResponseEntity.ok(response);
    }

    /**
     * 商品を更新
     */
    @PutMapping("/{productCode}")
    @Operation(summary = "商品更新", description = "指定された商品を更新します")
    public ResponseEntity<ProductResponse> updateProduct(
            @PathVariable String productCode,
            @Valid @RequestBody UpdateProductRequest request) {
        ProductResponse response =
            productService.updateProduct(productCode, request);
        return ResponseEntity.ok(response);
    }

    /**
     * 商品を削除
     */
    @DeleteMapping("/{productCode}")
    @Operation(summary = "商品削除", description = "指定された商品を削除します")
    public ResponseEntity<Void> deleteProduct(
            @PathVariable String productCode) {
        productService.deleteProduct(productCode);
        return ResponseEntity.noContent().build();
    }
}
