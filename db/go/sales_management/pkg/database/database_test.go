package database

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/k2works/sales-management-db/test"
)

func TestConnectWithTestcontainers(t *testing.T) {
	// testcontainersでPostgreSQLコンテナを起動
	container := test.SetupPostgreSQLContainer(t)
	db := container.DB

	t.Run("データベース接続が正常に確立される", func(t *testing.T) {
		// 接続確認
		err := db.Ping()
		require.NoError(t, err)
	})

	t.Run("シンプルなクエリを実行できる", func(t *testing.T) {
		var result int
		err := db.Get(&result, "SELECT 1")
		require.NoError(t, err)
		assert.Equal(t, 1, result)
	})

	t.Run("テーブルを作成してデータを挿入できる", func(t *testing.T) {
		// テーブル作成
		_, err := db.Exec(`
			CREATE TABLE test_users (
				id SERIAL PRIMARY KEY,
				name VARCHAR(100) NOT NULL,
				email VARCHAR(100) NOT NULL
			)
		`)
		require.NoError(t, err)

		// データ挿入
		result, err := db.Exec(
			"INSERT INTO test_users (name, email) VALUES ($1, $2)",
			"Test User", "test@example.com",
		)
		require.NoError(t, err)

		// 挿入確認
		rowsAffected, err := result.RowsAffected()
		require.NoError(t, err)
		assert.Equal(t, int64(1), rowsAffected)

		// データ取得
		var count int
		err = db.Get(&count, "SELECT COUNT(*) FROM test_users")
		require.NoError(t, err)
		assert.Equal(t, 1, count)
	})
}

func TestConnect(t *testing.T) {
	t.Run("DATABASE_URLが空の場合エラーを返す", func(t *testing.T) {
		config := &Config{
			DatabaseURL: "",
		}

		db, err := Connect(config)
		assert.Error(t, err)
		assert.Nil(t, db)
		assert.Contains(t, err.Error(), "DATABASE_URL is not set")
	})

	t.Run("無効なDATABASE_URLの場合エラーを返す", func(t *testing.T) {
		config := &Config{
			DatabaseURL: "invalid://connection",
		}

		db, err := Connect(config)
		assert.Error(t, err)
		assert.Nil(t, db)
	})
}
