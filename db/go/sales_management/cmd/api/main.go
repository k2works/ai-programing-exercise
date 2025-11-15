package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/k2works/sales-management-db/internal/api/handler"
	"github.com/k2works/sales-management-db/internal/api/service"
	"github.com/k2works/sales-management-db/internal/repository"
	"github.com/k2works/sales-management-db/pkg/database"

	"github.com/gin-gonic/gin"
	swaggerFiles "github.com/swaggo/files"
	ginSwagger "github.com/swaggo/gin-swagger"
)

// @title 販売管理システム API
// @version 1.0
// @description sqlx + Gin による販売管理システムの REST API
// @host localhost:8080
// @BasePath /
func main() {
	// データベース接続
	db, err := database.New("host=localhost port=5432 user=postgres password=password dbname=sales_management sslmode=disable")
	if err != nil {
		log.Fatalf("Failed to connect to database: %v", err)
	}
	defer db.Close()

	// Infrastructure 層の初期化
	productRepo := repository.NewAPIProductRepository()

	// Service 層の初期化
	productService := service.NewProductService(productRepo)

	// Presentation 層の初期化
	productHandler := handler.NewProductHandler(productService, db)

	// Gin ルーターの設定
	router := gin.Default()

	// ヘルスチェックエンドポイント
	router.GET("/health", func(c *gin.Context) {
		ctx := c.Request.Context()
		// データベース接続確認
		var result int
		err := db.QueryRowxContext(ctx, "SELECT 1").Scan(&result)
		if err != nil {
			c.JSON(http.StatusServiceUnavailable, gin.H{"status": "error", "message": err.Error()})
			return
		}
		c.JSON(http.StatusOK, gin.H{"status": "ok"})
	})

	// API v1 グループ
	v1 := router.Group("/api/v1")
	{
		products := v1.Group("/products")
		{
			products.POST("", productHandler.CreateProduct)
			products.GET("", productHandler.GetAllProducts)
			products.GET("/:prodCode", productHandler.GetProduct)
			products.PUT("/:prodCode", productHandler.UpdateProduct)
			products.DELETE("/:prodCode", productHandler.DeleteProduct)
		}
	}

	// Swagger UI
	router.GET("/swagger/*any", ginSwagger.WrapHandler(swaggerFiles.Handler))

	// サーバー起動
	fmt.Println("🚀 Server started at http://0.0.0.0:8080/")
	fmt.Println("📍 Endpoints:")
	fmt.Println("  POST   /api/v1/products")
	fmt.Println("  GET    /api/v1/products")
	fmt.Println("  GET    /api/v1/products/:prodCode")
	fmt.Println("  PUT    /api/v1/products/:prodCode")
	fmt.Println("  DELETE /api/v1/products/:prodCode")
	fmt.Println("  GET    /health")
	fmt.Println()
	fmt.Println("📖 Swagger UI: http://0.0.0.0:8080/swagger/index.html")
	fmt.Println()

	if err := router.Run(":8080"); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}
