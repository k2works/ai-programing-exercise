package schema

import "github.com/k2works/sales-management-db/internal/model"

// CreateProductRequest 商品作成リクエスト
type CreateProductRequest struct {
	ProdCode     string `json:"prodCode" binding:"required" example:"PROD00001"`
	FullName     string `json:"fullName" binding:"required" example:"黒毛和牛サーロインステーキ 200g"`
	Name         string `json:"name" binding:"required" example:"サーロイン"`
	Kana         string `json:"kana" binding:"required" example:"クロゲワギュウサーロイン"`
	UnitPrice    int    `json:"unitPrice" binding:"required,min=0" example:"5000"`
	PoPrice      int    `json:"poPrice" binding:"required,min=0" example:"3500"`
	SupCode      string `json:"supCode" binding:"required" example:"COMP0011"`
	CategoryCode string `json:"categoryCode" binding:"required" example:"CAT001"`
}

// UpdateProductRequest 商品更新リクエスト（すべてオプション）
type UpdateProductRequest struct {
	FullName     *string `json:"fullName,omitempty" example:"黒毛和牛サーロインステーキ 200g"`
	Name         *string `json:"name,omitempty" example:"サーロイン"`
	Kana         *string `json:"kana,omitempty" example:"クロゲワギュウサーロイン"`
	UnitPrice    *int    `json:"unitPrice,omitempty" example:"5000"`
	PoPrice      *int    `json:"poPrice,omitempty" example:"3500"`
	SupCode      *string `json:"supCode,omitempty" example:"COMP0011"`
	CategoryCode *string `json:"categoryCode,omitempty" example:"CAT001"`
}

// ProductResponse 商品レスポンス
type ProductResponse struct {
	ProdCode     string `json:"prodCode" example:"PROD00001"`
	FullName     string `json:"fullName" example:"黒毛和牛サーロインステーキ 200g"`
	Name         string `json:"name" example:"サーロイン"`
	Kana         string `json:"kana" example:"クロゲワギュウサーロイン"`
	UnitPrice    int    `json:"unitPrice" example:"5000"`
	PoPrice      int    `json:"poPrice" example:"3500"`
	SupCode      string `json:"supCode" example:"COMP0011"`
	CategoryCode string `json:"categoryCode" example:"CAT001"`
}

// ProductResponseFromModel ドメインモデルから変換
func ProductResponseFromModel(product *model.Product) ProductResponse {
	return ProductResponse{
		ProdCode:     product.ProductCode,
		FullName:     product.ProductFullName,
		Name:         product.ProductAbbreviation,
		Kana:         product.ProductNameKana,
		UnitPrice:    product.SellingPrice,
		PoPrice:      product.PurchasePrice,
		SupCode:      product.SupplierCode,
		CategoryCode: product.ProductCategoryCode,
	}
}

// ErrorResponse エラーレスポンス
type ErrorResponse struct {
	Error   string  `json:"error" example:"エラーメッセージ"`
	Details *string `json:"details,omitempty" example:"詳細情報"`
}
