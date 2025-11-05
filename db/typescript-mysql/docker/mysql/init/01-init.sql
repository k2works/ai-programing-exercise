-- MySQL 初期化スクリプト
-- データベースの初期設定を行う

-- 文字コードの確認
SELECT @@character_set_database, @@collation_database;

-- タイムゾーンの設定
SET time_zone = '+09:00';

-- テスト用データベースを作成
CREATE DATABASE IF NOT EXISTS sales_management_test CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

-- 権限を付与
GRANT ALL PRIVILEGES ON sales_management.* TO 'user'@'%';
GRANT ALL PRIVILEGES ON sales_management_test.* TO 'user'@'%';
-- Prisma shadow database の権限（migrate dev 用）
GRANT ALL PRIVILEGES ON `prisma_migrate_shadow_db_%`.* TO 'user'@'%';
-- ストアドプロシージャの権限
GRANT CREATE ROUTINE, ALTER ROUTINE, EXECUTE ON sales_management.* TO 'user'@'%';
GRANT CREATE ROUTINE, ALTER ROUTINE, EXECUTE ON sales_management_test.* TO 'user'@'%';
FLUSH PRIVILEGES;

-- ログ出力
SELECT 'MySQL database initialized successfully' AS message;
