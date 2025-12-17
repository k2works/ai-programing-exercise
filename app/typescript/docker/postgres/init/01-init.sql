-- PostgreSQL 初期化スクリプト
-- 生産管理システムデータベースの初期設定を行う

-- タイムゾーンの設定
SET timezone = 'Asia/Tokyo';

-- ログ出力
DO $$
BEGIN
    RAISE NOTICE 'PostgreSQL database for production management initialized successfully';
END $$;
