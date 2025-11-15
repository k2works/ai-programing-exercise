-- MySQL 初期化スクリプト
-- データベースの初期設定を行う

-- 文字コードの確認
SELECT @@character_set_database, @@collation_database;

-- タイムゾーンの設定
SET time_zone = '+09:00';

-- ログ出力
SELECT 'MySQL database initialized successfully' AS message;
