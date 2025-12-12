-- V6__create_audit_log_table.sql
CREATE TABLE IF NOT EXISTS audit_log (
    id BIGSERIAL PRIMARY KEY,
    entity_type VARCHAR(50) NOT NULL,
    entity_id VARCHAR(100) NOT NULL,
    action VARCHAR(20) NOT NULL CHECK (action IN ('CREATE', 'UPDATE', 'DELETE')),
    user_id VARCHAR(100) NOT NULL,
    user_name VARCHAR(200) NOT NULL,
    timestamp TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    old_values JSONB,
    new_values JSONB,
    changes JSONB,
    reason TEXT,
    ip_address VARCHAR(45),
    user_agent TEXT,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス作成（効率的な検索のため）
CREATE INDEX idx_audit_log_entity ON audit_log(entity_type, entity_id);
CREATE INDEX idx_audit_log_user ON audit_log(user_id);
CREATE INDEX idx_audit_log_timestamp ON audit_log(timestamp DESC);
CREATE INDEX idx_audit_log_action ON audit_log(action);
CREATE INDEX idx_audit_log_changes ON audit_log USING GIN (changes);

-- コメント
COMMENT ON TABLE audit_log IS '監査ログテーブル（Append-Onlyで不変）';
COMMENT ON COLUMN audit_log.entity_type IS '監査対象エンティティタイプ（例：Account, Journal）';
COMMENT ON COLUMN audit_log.entity_id IS '監査対象エンティティID';
COMMENT ON COLUMN audit_log.action IS '操作種別（CREATE/UPDATE/DELETE）';
COMMENT ON COLUMN audit_log.old_values IS '変更前の値（JSON形式）';
COMMENT ON COLUMN audit_log.new_values IS '変更後の値（JSON形式）';
COMMENT ON COLUMN audit_log.changes IS '変更された項目のみ（JSON形式）';

-- 権限制限（Append-Onlyを保証）
-- 実運用では、アプリケーションユーザーには SELECT と INSERT のみを許可
-- REVOKE UPDATE, DELETE ON audit_log FROM app_user;
