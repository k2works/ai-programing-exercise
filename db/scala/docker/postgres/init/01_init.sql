-- データベースの初期設定
-- 必要に応じて拡張機能などを有効化

-- UUID 拡張の有効化（必要な場合）
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- タイムゾーンの設定
SET timezone = 'Asia/Tokyo';
