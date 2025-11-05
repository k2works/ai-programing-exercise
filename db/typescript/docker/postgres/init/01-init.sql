-- PostgreSQL 初期化スクリプト
-- データベースの初期設定を行う

-- 拡張機能の有効化（必要に応じて）
-- CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
-- CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- タイムゾーンの設定
SET timezone = 'Asia/Tokyo';

-- ログ出力
DO $$
BEGIN
    RAISE NOTICE 'PostgreSQL database initialized successfully';
END $$;
