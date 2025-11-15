package schema

import (
	"encoding/json"
	"testing"
	"time"

	"github.com/k2works/sales-management-db/internal/model"
)

func TestCreateProductRequest_JSON(t *testing.T) {
	jsonStr := `{
		"prodCode": "PROD00001",
		"fullName": "黒毛和牛サーロインステーキ 200g",
		"name": "サーロイン",
		"kana": "クロゲワギュウサーロイン",
		"unitPrice": 5000,
		"poPrice": 3500,
		"supCode": "COMP0011",
		"categoryCode": "CAT001"
	}`

	var req CreateProductRequest
	err := json.Unmarshal([]byte(jsonStr), &req)
	if err != nil {
		t.Fatalf("Failed to unmarshal JSON: %v", err)
	}

	if req.ProdCode != "PROD00001" {
		t.Errorf("Expected ProdCode PROD00001, got %s", req.ProdCode)
	}

	if req.UnitPrice != 5000 {
		t.Errorf("Expected UnitPrice 5000, got %d", req.UnitPrice)
	}
}

func TestProductResponse_JSON(t *testing.T) {
	response := ProductResponse{
		ProdCode:     "PROD00001",
		FullName:     "黒毛和牛サーロインステーキ 200g",
		Name:         "サーロイン",
		Kana:         "クロゲワギュウサーロイン",
		UnitPrice:    5000,
		PoPrice:      3500,
		SupCode:      "COMP0011",
		CategoryCode: "CAT001",
	}

	jsonBytes, err := json.Marshal(response)
	if err != nil {
		t.Fatalf("Failed to marshal to JSON: %v", err)
	}

	var decoded ProductResponse
	err = json.Unmarshal(jsonBytes, &decoded)
	if err != nil {
		t.Fatalf("Failed to unmarshal JSON: %v", err)
	}

	if decoded.ProdCode != "PROD00001" {
		t.Errorf("Expected ProdCode PROD00001, got %s", decoded.ProdCode)
	}

	if decoded.UnitPrice != 5000 {
		t.Errorf("Expected UnitPrice 5000, got %d", decoded.UnitPrice)
	}
}

func TestProductResponse_FromModel(t *testing.T) {
	now := time.Now()
	product := &model.Product{
		ProductCode:         "PROD00001",
		ProductFullName:     "黒毛和牛サーロインステーキ 200g",
		ProductAbbreviation: "サーロイン",
		ProductNameKana:     "クロゲワギュウサーロイン",
		SellingPrice:        5000,
		PurchasePrice:       3500,
		SupplierCode:        "COMP0011",
		ProductCategoryCode: "CAT001",
		CreatedAt:           now,
		CreatedBy:           "test",
		UpdatedAt:           now,
		UpdatedBy:           "test",
	}

	response := ProductResponseFromModel(product)

	if response.ProdCode != "PROD00001" {
		t.Errorf("Expected ProdCode PROD00001, got %s", response.ProdCode)
	}

	if response.UnitPrice != 5000 {
		t.Errorf("Expected UnitPrice 5000, got %d", response.UnitPrice)
	}
}
