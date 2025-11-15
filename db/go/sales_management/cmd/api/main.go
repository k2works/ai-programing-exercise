package main

import (
	"fmt"
	"log"
	"net/http"

	"github.com/k2works/sales-management-db/internal/api/handler"
	"github.com/k2works/sales-management-db/internal/api/service"
	"github.com/k2works/sales-management-db/internal/repository"
	"github.com/k2works/sales-management-db/pkg/database"

	_ "github.com/k2works/sales-management-db/docs" // Swagger ドキュメント

	"github.com/gin-gonic/gin"
	"github.com/gin-contrib/cors"
	swaggerFiles "github.com/swaggo/files"
	ginSwagger "github.com/swaggo/gin-swagger"
)

// @title 販売管理システム API
// @version 1.0
// @description sqlx + Gin による販売管理システムの REST API
// @host localhost:9000
// @BasePath /
// @schemes http
func main() {
	// データベース接続
	config := database.NewConfig()
	if config.DatabaseURL == "" {
		config.DatabaseURL = "host=localhost port=5432 user=postgres password=password dbname=sales_management sslmode=disable"
	}
	db, err := database.Connect(config)
	if err != nil {
		log.Fatalf("Failed to connect to database: %v", err)
	}
	defer database.Close(db)

	// Infrastructure 層の初期化
	productRepo := repository.NewProductRepository(db)

	// Service 層の初期化
	productService := service.NewProductServiceV2(productRepo)

	// Presentation 層の初期化
	productHandler := handler.NewProductHandlerV2(productService, db)

	// Gin ルーターの設定
	router := gin.Default()

	// CORS 設定
	router.Use(cors.New(cors.Config{
		AllowAllOrigins:  true,
		AllowMethods:     []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowHeaders:     []string{"Origin", "Content-Type", "Accept", "Authorization"},
		ExposeHeaders:    []string{"Content-Length"},
		AllowCredentials: false,
	}))

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
	fmt.Println("🚀 Server started at http://0.0.0.0:9000/")
	fmt.Println("📍 Endpoints:")
	fmt.Println("  POST   /api/v1/products")
	fmt.Println("  GET    /api/v1/products")
	fmt.Println("  GET    /api/v1/products/:prodCode")
	fmt.Println("  PUT    /api/v1/products/:prodCode")
	fmt.Println("  DELETE /api/v1/products/:prodCode")
	fmt.Println("  GET    /health")
	fmt.Println()
	fmt.Println("📖 Swagger UI: http://0.0.0.0:9000/swagger/index.html")
	fmt.Println()

	if err := router.Run(":9000"); err != nil {
		log.Fatalf("Failed to start server: %v", err)
	}
}
