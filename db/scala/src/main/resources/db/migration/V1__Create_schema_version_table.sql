-- 初期マイグレーション
-- Flywayが自動的に flyway_schema_history テーブルを作成するため、
-- ここでは空のマイグレーションとして記録のみ行います

-- スキーマバージョン確認用のコメント
-- このマイグレーションが適用されたことを示すマーカー
SELECT 'Initial schema setup' AS status;
