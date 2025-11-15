package handler

import (
	"net/http"

	"github.com/k2works/sales-management-db/internal/api/schema"
	"github.com/k2works/sales-management-db/internal/api/service"

	"github.com/gin-gonic/gin"
	"github.com/jmoiron/sqlx"
)

const (
	// ErrProductNotFound 商品が見つからない場合のエラーメッセージ
	ErrProductNotFound = "商品が見つかりません"
)

// ProductHandlerV2 商品ハンドラ（既存リポジトリ使用）
type ProductHandlerV2 struct {
	service *service.ProductServiceV2
	db      *sqlx.DB
}

// NewProductHandlerV2 商品ハンドラを作成
func NewProductHandlerV2(service *service.ProductServiceV2, db *sqlx.DB) *ProductHandlerV2 {
	return &ProductHandlerV2{
		service: service,
		db:      db,
	}
}

// CreateProduct godoc
// @Summary 商品作成
// @Description 新しい商品を作成します
// @Tags products
// @Accept json
// @Produce json
// @Param product body schema.CreateProductRequest true "商品作成リクエスト"
// @Success 201 {object} schema.ProductResponse
// @Failure 400 {object} schema.ErrorResponse
// @Router /api/v1/products [post]
func (h *ProductHandlerV2) CreateProduct(c *gin.Context) {
	var request schema.CreateProductRequest
	if err := c.ShouldBindJSON(&request); err != nil {
		c.JSON(http.StatusBadRequest, schema.ErrorResponse{Error: err.Error()})
		return
	}

	ctx := c.Request.Context()
	tx, err := h.db.Beginx()
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}
	defer tx.Rollback()

	product, err := h.service.CreateProduct(ctx, tx, request)
	if err != nil {
		c.JSON(http.StatusBadRequest, schema.ErrorResponse{Error: err.Error()})
		return
	}

	if err := tx.Commit(); err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}

	c.JSON(http.StatusCreated, product)
}

// GetAllProducts godoc
// @Summary 商品一覧取得
// @Description すべての商品を取得します
// @Tags products
// @Produce json
// @Success 200 {array} schema.ProductResponse
// @Router /api/v1/products [get]
func (h *ProductHandlerV2) GetAllProducts(c *gin.Context) {
	ctx := c.Request.Context()

	products, err := h.service.GetAllProducts(ctx, h.db)
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}

	c.JSON(http.StatusOK, products)
}

// GetProduct godoc
// @Summary 商品詳細取得
// @Description 指定された商品コードの商品を取得します
// @Tags products
// @Produce json
// @Param prodCode path string true "商品コード"
// @Success 200 {object} schema.ProductResponse
// @Failure 404 {object} schema.ErrorResponse
// @Router /api/v1/products/{prodCode} [get]
func (h *ProductHandlerV2) GetProduct(c *gin.Context) {
	prodCode := c.Param("prodCode")
	ctx := c.Request.Context()

	product, err := h.service.GetProductByCode(ctx, h.db, prodCode)
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}

	if product == nil {
		c.JSON(http.StatusNotFound, schema.ErrorResponse{Error: ErrProductNotFound})
		return
	}

	c.JSON(http.StatusOK, product)
}

// UpdateProduct godoc
// @Summary 商品更新
// @Description 指定された商品コードの商品を更新します
// @Tags products
// @Accept json
// @Produce json
// @Param prodCode path string true "商品コード"
// @Param product body schema.UpdateProductRequest true "商品更新リクエスト"
// @Success 200 {object} schema.ProductResponse
// @Failure 400 {object} schema.ErrorResponse
// @Failure 404 {object} schema.ErrorResponse
// @Router /api/v1/products/{prodCode} [put]
func (h *ProductHandlerV2) UpdateProduct(c *gin.Context) {
	prodCode := c.Param("prodCode")

	var request schema.UpdateProductRequest
	if err := c.ShouldBindJSON(&request); err != nil {
		c.JSON(http.StatusBadRequest, schema.ErrorResponse{Error: err.Error()})
		return
	}

	ctx := c.Request.Context()
	tx, err := h.db.Beginx()
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}
	defer tx.Rollback()

	product, err := h.service.UpdateProduct(ctx, tx, prodCode, request)
	if err != nil {
		if err.Error() == ErrProductNotFound {
			c.JSON(http.StatusNotFound, schema.ErrorResponse{Error: err.Error()})
		} else {
			c.JSON(http.StatusBadRequest, schema.ErrorResponse{Error: err.Error()})
		}
		return
	}

	if err := tx.Commit(); err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}

	c.JSON(http.StatusOK, product)
}

// DeleteProduct godoc
// @Summary 商品削除
// @Description 指定された商品コードの商品を削除します
// @Tags products
// @Param prodCode path string true "商品コード"
// @Success 204
// @Failure 404 {object} schema.ErrorResponse
// @Router /api/v1/products/{prodCode} [delete]
func (h *ProductHandlerV2) DeleteProduct(c *gin.Context) {
	prodCode := c.Param("prodCode")
	ctx := c.Request.Context()

	tx, err := h.db.Beginx()
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}
	defer tx.Rollback()

	err = h.service.DeleteProduct(ctx, tx, prodCode)
	if err != nil {
		if err.Error() == ErrProductNotFound {
			c.JSON(http.StatusNotFound, schema.ErrorResponse{Error: err.Error()})
		} else {
			c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		}
		return
	}

	if err := tx.Commit(); err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}

	c.Status(http.StatusNoContent)
}
