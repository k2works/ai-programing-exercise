//! 商品DTOの定義

use serde::{Deserialize, Serialize};
use utoipa::ToSchema;
use validator::Validate;

/// 商品作成リクエスト
#[derive(Debug, Clone, Serialize, Deserialize, Validate, ToSchema)]
pub struct CreateProductRequest {
    /// 商品コード（16文字以内）
    #[validate(length(min = 1, max = 16))]
    #[schema(example = "PROD00001")]
    pub prod_code: String,

    /// 商品分類コード（8文字以内）
    #[validate(length(max = 8))]
    #[schema(example = "CAT001")]
    pub prod_category_code: Option<String>,

    /// 商品正式名（40文字以内）
    #[validate(length(min = 1, max = 40))]
    #[schema(example = "黒毛和牛サーロインステーキ 200g")]
    pub fullname: String,

    /// 商品略称（10文字以内）
    #[validate(length(min = 1, max = 10))]
    #[schema(example = "サーロイン")]
    pub name: String,

    /// 商品名カナ（20文字以内）
    #[validate(length(min = 1, max = 20))]
    #[schema(example = "クロゲワギュウサーロイン")]
    pub kana: String,

    /// 商品区分
    #[validate(length(max = 1))]
    #[schema(example = "1")]
    pub prod_class: Option<String>,

    /// 製品型番
    #[validate(length(max = 40))]
    pub model_number: Option<String>,

    /// 販売単価
    #[validate(range(min = 0))]
    #[schema(example = 5000)]
    pub unitprice: i32,

    /// 仕入単価
    #[validate(range(min = 0))]
    #[schema(example = 3000)]
    pub purchase_price: Option<i32>,

    /// 売上原価
    #[validate(range(min = 0))]
    #[schema(example = 3500)]
    pub prime_cost: i32,

    /// 税区分（1:課税、2:非課税）
    #[validate(range(min = 1, max = 2))]
    #[schema(example = 1)]
    pub tax_class: Option<i32>,

    /// 在庫管理対象区分
    pub stock_managed: Option<i32>,

    /// 在庫引当区分
    pub stock_reserve: Option<i32>,

    /// 仕入先コード
    #[validate(length(min = 1, max = 8))]
    #[schema(example = "COMP0011")]
    pub sup_code: String,

    /// 仕入先枝番
    #[schema(example = 1)]
    pub sup_seq_num: Option<i32>,
}

impl CreateProductRequest {
    /// ビジネスルールのバリデーション
    pub fn validate_business_rules(&self) -> Result<(), String> {
        if self.unitprice < self.prime_cost {
            return Err("販売単価が売上原価より低い設定はできません".to_string());
        }
        Ok(())
    }
}

/// 商品更新リクエスト（すべてオプション）
#[derive(Debug, Clone, Default, Serialize, Deserialize, Validate, ToSchema)]
pub struct UpdateProductRequest {
    /// 商品分類コード
    #[validate(length(max = 8))]
    pub prod_category_code: Option<String>,

    /// 商品正式名
    #[validate(length(min = 1, max = 40))]
    #[schema(example = "黒毛和牛サーロインステーキ 250g")]
    pub fullname: Option<String>,

    /// 商品略称
    #[validate(length(min = 1, max = 10))]
    pub name: Option<String>,

    /// 商品名カナ
    #[validate(length(min = 1, max = 20))]
    pub kana: Option<String>,

    /// 商品区分
    #[validate(length(max = 1))]
    pub prod_class: Option<String>,

    /// 製品型番
    #[validate(length(max = 40))]
    pub model_number: Option<String>,

    /// 販売単価
    #[validate(range(min = 0))]
    #[schema(example = 5500)]
    pub unitprice: Option<i32>,

    /// 仕入単価
    #[validate(range(min = 0))]
    pub purchase_price: Option<i32>,

    /// 売上原価
    #[validate(range(min = 0))]
    pub prime_cost: Option<i32>,

    /// 税区分
    #[validate(range(min = 1, max = 2))]
    pub tax_class: Option<i32>,

    /// 在庫管理対象区分
    pub stock_managed: Option<i32>,

    /// 在庫引当区分
    pub stock_reserve: Option<i32>,

    /// 仕入先コード
    #[validate(length(min = 1, max = 8))]
    pub sup_code: Option<String>,

    /// 仕入先枝番
    pub sup_seq_num: Option<i32>,
}

/// 商品レスポンス
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct ProductResponse {
    /// 商品コード
    #[schema(example = "PROD00001")]
    pub prod_code: String,

    /// 商品分類コード
    #[schema(example = "CAT001")]
    pub prod_category_code: Option<String>,

    /// 商品正式名
    #[schema(example = "黒毛和牛サーロインステーキ 200g")]
    pub fullname: String,

    /// 商品略称
    #[schema(example = "サーロイン")]
    pub name: String,

    /// 商品名カナ
    #[schema(example = "クロゲワギュウサーロイン")]
    pub kana: String,

    /// 商品区分
    pub prod_class: Option<String>,

    /// 製品型番
    pub model_number: Option<String>,

    /// 販売単価
    #[schema(example = 5000)]
    pub unitprice: i32,

    /// 仕入単価
    pub purchase_price: Option<i32>,

    /// 売上原価
    #[schema(example = 3500)]
    pub prime_cost: i32,

    /// 税区分
    pub tax_class: i32,

    /// 雑区分
    pub misc_class: Option<i32>,

    /// 在庫管理対象区分
    #[schema(example = 1)]
    pub stock_managed: Option<i32>,

    /// 在庫引当区分
    pub stock_reserve: Option<i32>,

    /// 仕入先コード
    #[schema(example = "COMP0011")]
    pub sup_code: String,

    /// 仕入先枝番
    #[schema(example = 1)]
    pub sup_seq_num: Option<i32>,

    /// 作成日時
    pub created_at: chrono::NaiveDateTime,

    /// 更新日時
    pub updated_at: chrono::NaiveDateTime,
}
