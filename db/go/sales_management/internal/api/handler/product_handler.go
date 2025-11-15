package handler

import (
	"net/http"

	"github.com/k2works/sales-management-db/internal/api/schema"
	"github.com/k2works/sales-management-db/internal/api/service"
	"github.com/k2works/sales-management-db/pkg/database"

	"github.com/gin-gonic/gin"
)

// ProductHandler 商品ハンドラ
type ProductHandler struct {
	service *service.ProductService
	db      *database.DB
}

// NewProductHandler 商品ハンドラを作成
func NewProductHandler(service *service.ProductService, db *database.DB) *ProductHandler {
	return &ProductHandler{
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
func (h *ProductHandler) CreateProduct(c *gin.Context) {
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

	txWrapped := &database.Tx{Tx: tx}
	product, err := h.service.CreateProduct(ctx, txWrapped, request)
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
func (h *ProductHandler) GetAllProducts(c *gin.Context) {
	ctx := c.Request.Context()

	products, err := h.service.GetAllProducts(ctx, h.db.DB)
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
func (h *ProductHandler) GetProduct(c *gin.Context) {
	prodCode := c.Param("prodCode")
	ctx := c.Request.Context()

	product, err := h.service.GetProductByCode(ctx, h.db.DB, prodCode)
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}

	if product == nil {
		c.JSON(http.StatusNotFound, schema.ErrorResponse{Error: "商品が見つかりません"})
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
func (h *ProductHandler) UpdateProduct(c *gin.Context) {
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

	txWrapped := &database.Tx{Tx: tx}
	product, err := h.service.UpdateProduct(ctx, txWrapped, prodCode, request)
	if err != nil {
		if err.Error() == "商品が見つかりません" {
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
func (h *ProductHandler) DeleteProduct(c *gin.Context) {
	prodCode := c.Param("prodCode")
	ctx := c.Request.Context()

	tx, err := h.db.Beginx()
	if err != nil {
		c.JSON(http.StatusInternalServerError, schema.ErrorResponse{Error: err.Error()})
		return
	}
	defer tx.Rollback()

	txWrapped := &database.Tx{Tx: tx}
	err = h.service.DeleteProduct(ctx, txWrapped, prodCode)
	if err != nil {
		if err.Error() == "商品が見つかりません" {
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
