//! Web API 統合テスト
//! Testcontainers で PostgreSQL を起動し、実際の HTTP リクエストで API をテスト

use axum::{
    routing::{delete, get, post, put},
    Router,
};
use sales_management_db::{
    dto::product::{CreateProductRequest, ProductResponse},
    entity::ProductCategory,
    repository::ProductCategoryRepository,
    service::ProductService,
    test_support::with_test_pool,
};
use serde_json::json;
use sqlx::PgPool;
use std::sync::Arc;
use tower::ServiceExt;

/// テスト用アプリケーションセットアップ（test_pool を使用）
fn setup_test_app(pool: PgPool) -> Router {
    // サービス層の初期化
    let product_service = Arc::new(ProductService::new(pool.clone()));

    // ルーター構築
    Router::new()
        .route("/products", post(sales_management_db::handler::product::create_product))
        .route("/products", get(sales_management_db::handler::product::get_all_products))
        .route(
            "/products/:prod_code",
            get(sales_management_db::handler::product::get_product_by_id),
        )
        .route("/products/:prod_code", put(sales_management_db::handler::product::update_product))
        .route(
            "/products/:prod_code",
            delete(sales_management_db::handler::product::delete_product),
        )
        .with_state(product_service)
}

/// テスト用商品分類を作成
async fn create_test_category(pool: &PgPool) -> ProductCategory {
    let category = ProductCategory {
        category_code: "CAT001".to_string(),
        name: Some("電子部品".to_string()),
        layer: 1,
        path: Some("/CAT001".to_string()),
        lowest_type: Some(1),
        create_date: chrono::Utc::now().naive_utc(),
        creator: Some("test".to_string()),
        update_date: chrono::Utc::now().naive_utc(),
        updater: Some("test".to_string()),
    };

    ProductCategoryRepository::create(pool, &category)
        .await
        .expect("Failed to create test category");

    category
}

#[tokio::test]
async fn test_create_product_success() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool);

        let request_body = json!({
            "prod_code": "PROD001",
            "prod_category_code": "CAT001",
            "fullname": "テスト商品フルネーム",
            "name": "テスト商品",
            "kana": "テストショウヒン",
            "prod_class": "1",
            "model_number": "SN-001",
            "unitprice": 1000,
            "purchase_price": 800,
            "prime_cost": 750,
            "tax_class": 1,
            "stock_managed": 1,
            "stock_reserve": 0,
            "sup_code": "SUP001",
            "sup_seq_num": 1
        });

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("POST")
                    .uri("/products")
                    .header("content-type", "application/json")
                    .body(axum::body::Body::from(serde_json::to_string(&request_body).unwrap()))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::CREATED);

        let body = axum::body::to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let product: ProductResponse = serde_json::from_slice(&body).unwrap();

        assert_eq!(product.prod_code, "PROD001");
        assert_eq!(product.name, "テスト商品");
        assert_eq!(product.unitprice, 1000);
    })
    .await;
}

#[tokio::test]
async fn test_create_product_validation_error() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool);

        // 販売単価が売上原価より低い（ビジネスルールエラー）
        let request_body = json!({
            "prod_code": "PROD002",
            "prod_category_code": "CAT001",
            "fullname": "エラー商品",
            "name": "エラー",
            "kana": "エラー",
            "unitprice": 500,
            "prime_cost": 1000,
            "sup_code": "SUP001"
        });

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("POST")
                    .uri("/products")
                    .header("content-type", "application/json")
                    .body(axum::body::Body::from(serde_json::to_string(&request_body).unwrap()))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    })
    .await;
}

#[tokio::test]
async fn test_get_all_products() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool.clone());

        // 商品を2つ作成
        let request1 = CreateProductRequest {
            prod_code: "PROD001".to_string(),
            prod_category_code: Some("CAT001".to_string()),
            fullname: "商品1".to_string(),
            name: "商品1".to_string(),
            kana: "ショウヒン1".to_string(),
            prod_class: Some("1".to_string()),
            model_number: None,
            unitprice: 1000,
            purchase_price: Some(800),
            prime_cost: 750,
            tax_class: Some(1),
            stock_managed: Some(1),
            stock_reserve: Some(0),
            sup_code: "SUP001".to_string(),
            sup_seq_num: Some(1),
        };

        let service = ProductService::new(pool.clone());
        service.create_product(request1).await.unwrap();

        let request2 = CreateProductRequest {
            prod_code: "PROD002".to_string(),
            prod_category_code: Some("CAT001".to_string()),
            fullname: "商品2".to_string(),
            name: "商品2".to_string(),
            kana: "ショウヒン2".to_string(),
            prod_class: Some("1".to_string()),
            model_number: None,
            unitprice: 2000,
            purchase_price: Some(1600),
            prime_cost: 1500,
            tax_class: Some(1),
            stock_managed: Some(1),
            stock_reserve: Some(0),
            sup_code: "SUP001".to_string(),
            sup_seq_num: Some(1),
        };

        service.create_product(request2).await.unwrap();

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("GET")
                    .uri("/products")
                    .body(axum::body::Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::OK);

        let body = axum::body::to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let products: Vec<ProductResponse> = serde_json::from_slice(&body).unwrap();

        assert_eq!(products.len(), 2);
        assert_eq!(products[0].prod_code, "PROD001");
        assert_eq!(products[1].prod_code, "PROD002");
    })
    .await;
}

#[tokio::test]
async fn test_get_product_by_id_success() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool.clone());

        // 商品を作成
        let request = CreateProductRequest {
            prod_code: "PROD001".to_string(),
            prod_category_code: Some("CAT001".to_string()),
            fullname: "商品1".to_string(),
            name: "商品1".to_string(),
            kana: "ショウヒン1".to_string(),
            prod_class: Some("1".to_string()),
            model_number: None,
            unitprice: 1000,
            purchase_price: Some(800),
            prime_cost: 750,
            tax_class: Some(1),
            stock_managed: Some(1),
            stock_reserve: Some(0),
            sup_code: "SUP001".to_string(),
            sup_seq_num: Some(1),
        };

        let service = ProductService::new(pool.clone());
        service.create_product(request).await.unwrap();

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("GET")
                    .uri("/products/PROD001")
                    .body(axum::body::Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::OK);

        let body = axum::body::to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let product: ProductResponse = serde_json::from_slice(&body).unwrap();

        assert_eq!(product.prod_code, "PROD001");
        assert_eq!(product.name, "商品1");
    })
    .await;
}

#[tokio::test]
async fn test_get_product_by_id_not_found() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool);

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("GET")
                    .uri("/products/NONEXISTENT")
                    .body(axum::body::Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::NOT_FOUND);
    })
    .await;
}

#[tokio::test]
async fn test_update_product_success() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool.clone());

        // 商品を作成
        let request = CreateProductRequest {
            prod_code: "PROD001".to_string(),
            prod_category_code: Some("CAT001".to_string()),
            fullname: "商品1".to_string(),
            name: "商品1".to_string(),
            kana: "ショウヒン1".to_string(),
            prod_class: Some("1".to_string()),
            model_number: None,
            unitprice: 1000,
            purchase_price: Some(800),
            prime_cost: 750,
            tax_class: Some(1),
            stock_managed: Some(1),
            stock_reserve: Some(0),
            sup_code: "SUP001".to_string(),
            sup_seq_num: Some(1),
        };

        let service = ProductService::new(pool.clone());
        service.create_product(request).await.unwrap();

        // 更新リクエスト
        let update_body = json!({
            "name": "更新商品",
            "unitprice": 1500
        });

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("PUT")
                    .uri("/products/PROD001")
                    .header("content-type", "application/json")
                    .body(axum::body::Body::from(serde_json::to_string(&update_body).unwrap()))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::OK);

        let body = axum::body::to_bytes(response.into_body(), usize::MAX).await.unwrap();
        let product: ProductResponse = serde_json::from_slice(&body).unwrap();

        assert_eq!(product.name, "更新商品");
        assert_eq!(product.unitprice, 1500);
    })
    .await;
}

#[tokio::test]
async fn test_update_product_business_rule_error() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool.clone());

        // 商品を作成
        let request = CreateProductRequest {
            prod_code: "PROD001".to_string(),
            prod_category_code: Some("CAT001".to_string()),
            fullname: "商品1".to_string(),
            name: "商品1".to_string(),
            kana: "ショウヒン1".to_string(),
            prod_class: Some("1".to_string()),
            model_number: None,
            unitprice: 1000,
            purchase_price: Some(800),
            prime_cost: 750,
            tax_class: Some(1),
            stock_managed: Some(1),
            stock_reserve: Some(0),
            sup_code: "SUP001".to_string(),
            sup_seq_num: Some(1),
        };

        let service = ProductService::new(pool.clone());
        service.create_product(request).await.unwrap();

        // 販売単価を売上原価より低く設定（ビジネスルールエラー）
        let update_body = json!({
            "unitprice": 500
        });

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("PUT")
                    .uri("/products/PROD001")
                    .header("content-type", "application/json")
                    .body(axum::body::Body::from(serde_json::to_string(&update_body).unwrap()))
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    })
    .await;
}

#[tokio::test]
async fn test_delete_product_success() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool.clone());

        // 商品を作成
        let request = CreateProductRequest {
            prod_code: "PROD001".to_string(),
            prod_category_code: Some("CAT001".to_string()),
            fullname: "商品1".to_string(),
            name: "商品1".to_string(),
            kana: "ショウヒン1".to_string(),
            prod_class: Some("1".to_string()),
            model_number: None,
            unitprice: 1000,
            purchase_price: Some(800),
            prime_cost: 750,
            tax_class: Some(1),
            stock_managed: Some(1),
            stock_reserve: Some(0),
            sup_code: "SUP001".to_string(),
            sup_seq_num: Some(1),
        };

        let service = ProductService::new(pool.clone());
        service.create_product(request).await.unwrap();

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("DELETE")
                    .uri("/products/PROD001")
                    .body(axum::body::Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::NO_CONTENT);
    })
    .await;
}

#[tokio::test]
async fn test_delete_product_not_found() {
    with_test_pool(|pool| async move {
        create_test_category(&pool).await;
        let app = setup_test_app(pool);

        let response = app
            .oneshot(
                axum::http::Request::builder()
                    .method("DELETE")
                    .uri("/products/NONEXISTENT")
                    .body(axum::body::Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), axum::http::StatusCode::INTERNAL_SERVER_ERROR);
    })
    .await;
}
