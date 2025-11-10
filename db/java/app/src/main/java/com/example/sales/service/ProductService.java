package com.example.sales.service;

import com.example.sales.domain.model.Product;
import com.example.sales.dto.*;
import com.example.sales.exception.ResourceNotFoundException;
import com.example.sales.exception.BusinessException;
import com.example.sales.domain.repository.ProductMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ProductService {

    private final ProductMapper productMapper;

    /**
     * 商品を作成
     */
    @Transactional
    public ProductResponse createProduct(CreateProductRequest request) {
        // ビジネスルール: 販売単価 >= 売上原価
        if (request.getUnitPrice() < request.getPrimeCost()) {
            throw new BusinessException(
                "販売単価が売上原価より低い設定はできません");
        }

        // 商品コードの重複チェック
        if (productMapper.existsById(request.getProductCode())) {
            throw new BusinessException(
                "商品コード " + request.getProductCode() + " は既に存在します");
        }

        Product product = new Product();
        product.setProductCode(request.getProductCode());
        product.setProductFormalName(request.getFullName());
        product.setProductAbbreviation(request.getName());
        product.setProductNameKana(request.getKanaName());
        product.setProductType("1");  // 商品区分のデフォルト値
        product.setProductCategoryCode("CAT001");  // 商品分類コードのデフォルト値
        product.setSellingPrice(request.getUnitPrice());
        product.setCostOfSales(request.getPrimeCost());
        product.setTaxType(1);  // 税区分のデフォルト値（課税）
        product.setInventoryManagementFlag(1);  // 在庫管理対象区分のデフォルト値
        product.setSupplierCode(request.getSupplierCode());
        product.setCreatedAt(LocalDateTime.now());
        product.setCreatedBy("SYSTEM");  // 実際は認証ユーザーから取得
        product.setUpdatedAt(LocalDateTime.now());
        product.setUpdatedBy("SYSTEM");

        productMapper.insert(product);
        return ProductResponse.fromEntity(product);
    }

    /**
     * すべての商品を取得
     */
    public List<ProductResponse> getAllProducts() {
        return productMapper.findAll().stream()
            .map(ProductResponse::fromEntity)
            .collect(Collectors.toList());
    }

    /**
     * ページング対応の商品一覧取得
     */
    public PageResponse<ProductResponse> getProducts(int page, int size) {
        int offset = page * size;
        List<Product> products = productMapper.findAllWithPaging(offset, size);
        int total = productMapper.count();

        List<ProductResponse> content = products.stream()
            .map(ProductResponse::fromEntity)
            .collect(Collectors.toList());

        return new PageResponse<>(content, page, size, total);
    }

    /**
     * IDで商品を取得
     */
    public ProductResponse getProductById(String productCode) {
        Product product = productMapper.findById(productCode)
            .orElseThrow(() -> new ResourceNotFoundException(
                "商品コード " + productCode + " が見つかりません"));

        return ProductResponse.fromEntity(product);
    }

    /**
     * 商品を更新
     */
    @Transactional
    public ProductResponse updateProduct(
            String productCode,
            UpdateProductRequest request) {

        Product existing = productMapper.findById(productCode)
            .orElseThrow(() -> new ResourceNotFoundException(
                "商品コード " + productCode + " が見つかりません"));

        // ビジネスルール: 販売単価 >= 売上原価
        Integer newUnitPrice = request.getUnitPrice() != null
            ? request.getUnitPrice() : existing.getSellingPrice();
        Integer newPrimeCost = request.getPrimeCost() != null
            ? request.getPrimeCost() : existing.getCostOfSales();

        if (newUnitPrice < newPrimeCost) {
            throw new BusinessException(
                "販売単価が売上原価より低い設定はできません");
        }

        // 更新項目の適用
        if (request.getFullName() != null) {
            existing.setProductFormalName(request.getFullName());
        }
        if (request.getName() != null) {
            existing.setProductAbbreviation(request.getName());
        }
        if (request.getKanaName() != null) {
            existing.setProductNameKana(request.getKanaName());
        }
        if (request.getUnitPrice() != null) {
            existing.setSellingPrice(request.getUnitPrice());
        }
        if (request.getPrimeCost() != null) {
            existing.setCostOfSales(request.getPrimeCost());
        }
        if (request.getSupplierCode() != null) {
            existing.setSupplierCode(request.getSupplierCode());
        }
        existing.setUpdatedAt(LocalDateTime.now());
        existing.setUpdatedBy("SYSTEM");

        productMapper.update(existing);
        return ProductResponse.fromEntity(existing);
    }

    /**
     * 商品を削除
     */
    @Transactional
    public void deleteProduct(String productCode) {
        if (!productMapper.existsById(productCode)) {
            throw new ResourceNotFoundException(
                "商品コード " + productCode + " が見つかりません");
        }

        productMapper.delete(productCode);
    }
}
