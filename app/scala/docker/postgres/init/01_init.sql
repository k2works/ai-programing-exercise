-- データベースの初期設定
-- 必要に応じて拡張機能などを有効化

-- UUID 拡張の有効化（必要な場合）
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- 全文検索用の拡張（日本語対応、必要な場合）
-- CREATE EXTENSION IF NOT EXISTS pg_trgm;

-- タイムゾーンの設定
SET timezone = 'Asia/Tokyo';
