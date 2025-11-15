package database

import (
	"fmt"
	"log"
	"os"

	"github.com/jmoiron/sqlx"
	_ "github.com/lib/pq" // PostgreSQL driver
)

// Config はデータベース接続設定を保持します
type Config struct {
	DatabaseURL string
}

// NewConfig は環境変数から設定を読み込みます
func NewConfig() *Config {
	return &Config{
		DatabaseURL: os.Getenv("DATABASE_URL"),
	}
}

// Connect はデータベースに接続します
func Connect(config *Config) (*sqlx.DB, error) {
	if config.DatabaseURL == "" {
		return nil, fmt.Errorf("DATABASE_URL is not set")
	}

	db, err := sqlx.Connect("postgres", config.DatabaseURL)
	if err != nil {
		return nil, fmt.Errorf("failed to connect to database: %w", err)
	}

	// 接続プールの設定
	db.SetMaxOpenConns(25)
	db.SetMaxIdleConns(5)

	log.Println("✅ データベース接続成功")
	return db, nil
}

// Close はデータベース接続を閉じます
func Close(db *sqlx.DB) error {
	if db != nil {
		log.Println("データベース接続をクローズします")
		return db.Close()
	}
	return nil
}
