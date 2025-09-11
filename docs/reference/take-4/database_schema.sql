-- ================================================================
-- 会議室予約システム データベーススキーマ
-- PostgreSQL 15+ 対応
-- ================================================================

-- データベース作成とセットアップ
-- ================================================================
CREATE DATABASE meeting_room_reservation_system
    ENCODING = 'UTF8'
    LC_COLLATE = 'ja_JP.UTF-8'  
    LC_CTYPE = 'ja_JP.UTF-8'
    TEMPLATE = template0;

\c meeting_room_reservation_system;

-- 拡張機能
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";

-- ENUM型定義
-- ================================================================
CREATE TYPE user_role AS ENUM ('MEMBER', 'STAFF', 'ADMIN');
CREATE TYPE user_status AS ENUM ('ACTIVE', 'INACTIVE', 'SUSPENDED');
CREATE TYPE room_status AS ENUM ('AVAILABLE', 'MAINTENANCE', 'DISABLED');
CREATE TYPE equipment_status AS ENUM ('AVAILABLE', 'DISABLED');
CREATE TYPE reservation_status AS ENUM ('CONFIRMED', 'CANCELLED', 'FORCE_CANCELLED', 'COMPLETED');
CREATE TYPE maintenance_status AS ENUM ('SCHEDULED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED');
CREATE TYPE usage_action AS ENUM ('CREATE', 'UPDATE', 'CANCEL', 'FORCE_CANCEL', 'COMPLETE');
CREATE TYPE inquiry_priority AS ENUM ('LOW', 'MEDIUM', 'HIGH', 'URGENT');
CREATE TYPE inquiry_status AS ENUM ('RECEIVED', 'IN_PROGRESS', 'RESOLVED', 'CLOSED');
CREATE TYPE response_action AS ENUM ('ACKNOWLEDGED', 'REPLIED', 'INVESTIGATED', 'COMPLETED', 'ESCALATED');

-- 共通関数
-- ================================================================

-- 更新日時自動更新関数
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    IF TG_TABLE_NAME = 'users' OR TG_TABLE_NAME = 'rooms' OR TG_TABLE_NAME = 'reservations' THEN
        NEW.version = OLD.version + 1;
    END IF;
    RETURN NEW;
END;
$$ language 'plpgsql';

-- 監査ログ関数
CREATE OR REPLACE FUNCTION audit_trigger_function()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        INSERT INTO audit_logs (table_name, operation, new_data, user_id)
        VALUES (TG_TABLE_NAME, TG_OP, row_to_json(NEW), 
               COALESCE(current_setting('app.current_user_id', true)::UUID, NEW.user_id));
        RETURN NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        INSERT INTO audit_logs (table_name, operation, old_data, new_data, user_id)
        VALUES (TG_TABLE_NAME, TG_OP, row_to_json(OLD), row_to_json(NEW), 
               COALESCE(current_setting('app.current_user_id', true)::UUID, NEW.user_id));
        RETURN NEW;
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO audit_logs (table_name, operation, old_data, user_id)
        VALUES (TG_TABLE_NAME, TG_OP, row_to_json(OLD), 
               current_setting('app.current_user_id', true)::UUID);
        RETURN OLD;
    END IF;
    RETURN NULL;
END;
$$ language 'plpgsql';

-- メインテーブル定義
-- ================================================================

-- 1. users（ユーザー情報）
CREATE TABLE users (
    user_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    full_name VARCHAR(100) NOT NULL,
    email VARCHAR(255) NOT NULL,
    phone_number VARCHAR(20),
    organization VARCHAR(100),
    role user_role NOT NULL DEFAULT 'MEMBER',
    status user_status NOT NULL DEFAULT 'ACTIVE',
    preferences JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 1
);

-- インデックス
CREATE UNIQUE INDEX idx_users_email ON users (email) WHERE status != 'INACTIVE';
CREATE INDEX idx_users_role_status ON users (role, status);
CREATE INDEX idx_users_created_at ON users (created_at DESC);
CREATE INDEX idx_users_preferences_gin ON users USING GIN (preferences);

-- 制約
ALTER TABLE users 
ADD CONSTRAINT chk_users_email_format 
CHECK (email ~* '^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$');

-- トリガー
CREATE TRIGGER trigger_users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- 2. login_info（ログイン情報）
CREATE TABLE login_info (
    login_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    user_id UUID NOT NULL REFERENCES users(user_id) ON DELETE CASCADE,
    password_hash VARCHAR(255) NOT NULL,
    salt VARCHAR(32) NOT NULL,
    last_login_at TIMESTAMP WITH TIME ZONE,
    password_updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    failed_attempts INTEGER NOT NULL DEFAULT 0,
    locked_until TIMESTAMP WITH TIME ZONE,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE UNIQUE INDEX idx_login_info_user_id ON login_info (user_id);
CREATE INDEX idx_login_info_locked_until ON login_info (locked_until) 
    WHERE locked_until IS NOT NULL;

-- 制約
ALTER TABLE login_info 
ADD CONSTRAINT chk_login_info_failed_attempts 
CHECK (failed_attempts >= 0 AND failed_attempts <= 10);

-- 3. rooms（会議室情報）
CREATE TABLE rooms (
    room_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    room_name VARCHAR(100) NOT NULL,
    capacity INTEGER NOT NULL,
    location VARCHAR(100) NOT NULL,
    description TEXT,
    status room_status NOT NULL DEFAULT 'AVAILABLE',
    floor_number INTEGER,
    attributes JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 1
);

-- インデックス
CREATE UNIQUE INDEX idx_rooms_room_name ON rooms (room_name);
CREATE INDEX idx_rooms_status_capacity ON rooms (status, capacity) 
    WHERE status = 'AVAILABLE';
CREATE INDEX idx_rooms_location ON rooms (location);
CREATE INDEX idx_rooms_attributes_gin ON rooms USING GIN (attributes);

-- 制約
ALTER TABLE rooms 
ADD CONSTRAINT chk_rooms_capacity 
CHECK (capacity > 0 AND capacity <= 100);

ALTER TABLE rooms 
ADD CONSTRAINT chk_rooms_floor_number 
CHECK (floor_number IS NULL OR (floor_number >= 1 AND floor_number <= 20));

-- トリガー
CREATE TRIGGER trigger_rooms_updated_at
    BEFORE UPDATE ON rooms
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- 4. equipment（設備情報）
CREATE TABLE equipment (
    equipment_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    equipment_name VARCHAR(50) NOT NULL,
    category VARCHAR(30) NOT NULL,
    description TEXT,
    status equipment_status NOT NULL DEFAULT 'AVAILABLE',
    specifications JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE UNIQUE INDEX idx_equipment_name_category ON equipment (equipment_name, category);
CREATE INDEX idx_equipment_status ON equipment (status);
CREATE INDEX idx_equipment_category ON equipment (category);

-- 5. room_equipment（会議室設備関連）
CREATE TABLE room_equipment (
    room_id UUID NOT NULL REFERENCES rooms(room_id) ON DELETE CASCADE,
    equipment_id UUID NOT NULL REFERENCES equipment(equipment_id) ON DELETE CASCADE,
    quantity INTEGER NOT NULL DEFAULT 1,
    notes VARCHAR(255),
    installed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (room_id, equipment_id)
);

-- インデックス
CREATE INDEX idx_room_equipment_equipment_id ON room_equipment (equipment_id);

-- 制約
ALTER TABLE room_equipment 
ADD CONSTRAINT chk_room_equipment_quantity 
CHECK (quantity > 0 AND quantity <= 10);

-- 6. reservations（予約情報）- パーティション対応
CREATE TABLE reservations (
    reservation_id UUID NOT NULL DEFAULT uuid_generate_v4(),
    user_id UUID NOT NULL REFERENCES users(user_id),
    room_id UUID NOT NULL REFERENCES rooms(room_id),
    start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
    end_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
    participant_count INTEGER NOT NULL,
    purpose TEXT,
    notes TEXT,
    status reservation_status NOT NULL DEFAULT 'CONFIRMED',
    metadata JSONB DEFAULT '{}',
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 1,
    PRIMARY KEY (reservation_id, start_datetime)
) PARTITION BY RANGE (start_datetime);

-- 現在月とその後の3ヶ月のパーティション作成
CREATE TABLE reservations_2025_09 PARTITION OF reservations
FOR VALUES FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE reservations_2025_10 PARTITION OF reservations
FOR VALUES FROM ('2025-10-01') TO ('2025-11-01');

CREATE TABLE reservations_2025_11 PARTITION OF reservations
FOR VALUES FROM ('2025-11-01') TO ('2025-12-01');

CREATE TABLE reservations_2025_12 PARTITION OF reservations
FOR VALUES FROM ('2025-12-01') TO ('2026-01-01');

-- インデックス
CREATE INDEX idx_reservations_user_start_desc ON reservations (user_id, start_datetime DESC, status);
CREATE INDEX idx_reservations_room_start ON reservations (room_id, start_datetime, end_datetime, status);
CREATE INDEX idx_reservations_status_start ON reservations (status, start_datetime);
CREATE INDEX idx_reservations_metadata_gin ON reservations USING GIN (metadata);

-- 一意制約（同時間帯重複防止）
CREATE UNIQUE INDEX idx_reservations_room_time_overlap ON reservations 
(room_id, start_datetime, end_datetime) 
WHERE status IN ('CONFIRMED');

-- アクティブ予約用部分インデックス
CREATE INDEX idx_reservations_active ON reservations (room_id, start_datetime)
WHERE status IN ('CONFIRMED') AND start_datetime > CURRENT_TIMESTAMP;

-- 制約
ALTER TABLE reservations 
ADD CONSTRAINT chk_reservations_time_order 
CHECK (end_datetime > start_datetime);

ALTER TABLE reservations 
ADD CONSTRAINT chk_reservations_participant_count 
CHECK (participant_count > 0);

-- トリガー
CREATE TRIGGER trigger_reservations_updated_at
    BEFORE UPDATE ON reservations
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- 7. cancellation_info（キャンセル情報）
CREATE TABLE cancellation_info (
    cancellation_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    reservation_id UUID NOT NULL,
    cancelled_by UUID NOT NULL REFERENCES users(user_id),
    reason TEXT,
    cancellation_type VARCHAR(20) NOT NULL DEFAULT 'USER',
    cancelled_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB DEFAULT '{}'
);

-- インデックス
CREATE INDEX idx_cancellation_info_reservation_id ON cancellation_info (reservation_id);
CREATE INDEX idx_cancellation_info_cancelled_by ON cancellation_info (cancelled_by);
CREATE INDEX idx_cancellation_info_cancelled_at ON cancellation_info (cancelled_at DESC);

-- 8. maintenance（メンテナンス情報）
CREATE TABLE maintenance (
    maintenance_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    room_id UUID NOT NULL REFERENCES rooms(room_id),
    title VARCHAR(200) NOT NULL,
    content TEXT,
    start_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
    end_datetime TIMESTAMP WITH TIME ZONE NOT NULL,
    status maintenance_status NOT NULL DEFAULT 'SCHEDULED',
    assigned_to VARCHAR(100),
    cost DECIMAL(10,2),
    notes TEXT,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX idx_maintenance_room_id ON maintenance (room_id);
CREATE INDEX idx_maintenance_status_start ON maintenance (status, start_datetime);
CREATE INDEX idx_maintenance_date_range ON maintenance (start_datetime, end_datetime);

-- 制約
ALTER TABLE maintenance 
ADD CONSTRAINT chk_maintenance_time_order 
CHECK (end_datetime > start_datetime);

-- 9. usage_history（利用履歴）- パーティション対応
CREATE TABLE usage_history (
    history_id UUID NOT NULL DEFAULT uuid_generate_v4(),
    user_id UUID NOT NULL REFERENCES users(user_id),
    reservation_id UUID,
    action usage_action NOT NULL,
    details JSONB DEFAULT '{}',
    ip_address INET,
    user_agent TEXT,
    executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (history_id, executed_at)
) PARTITION BY RANGE (executed_at);

-- 履歴用パーティション（月次）
CREATE TABLE usage_history_2025_09 PARTITION OF usage_history
FOR VALUES FROM ('2025-09-01') TO ('2025-10-01');

CREATE TABLE usage_history_2025_10 PARTITION OF usage_history
FOR VALUES FROM ('2025-10-01') TO ('2025-11-01');

-- インデックス
CREATE INDEX idx_usage_history_user_executed_desc ON usage_history (user_id, executed_at DESC);
CREATE INDEX idx_usage_history_reservation_id ON usage_history (reservation_id);
CREATE INDEX idx_usage_history_action_executed ON usage_history (action, executed_at);
CREATE INDEX idx_usage_history_details_gin ON usage_history USING GIN (details);

-- 10. inquiries（問い合わせ情報）
CREATE TABLE inquiries (
    inquiry_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    inquirer_name VARCHAR(100) NOT NULL,
    inquirer_email VARCHAR(255) NOT NULL,
    subject VARCHAR(200) NOT NULL,
    content TEXT NOT NULL,
    category VARCHAR(50),
    priority inquiry_priority NOT NULL DEFAULT 'MEDIUM',
    status inquiry_status NOT NULL DEFAULT 'RECEIVED',
    assigned_to UUID REFERENCES users(user_id),
    metadata JSONB DEFAULT '{}',
    received_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX idx_inquiries_status_priority ON inquiries (status, priority, received_at);
CREATE INDEX idx_inquiries_assigned_to ON inquiries (assigned_to);
CREATE INDEX idx_inquiries_email ON inquiries (inquirer_email);
CREATE INDEX idx_inquiries_received_at ON inquiries (received_at DESC);

-- 全文検索用インデックス
CREATE INDEX idx_inquiries_content_fts ON inquiries 
USING GIN (to_tsvector('japanese', subject || ' ' || content));

-- 11. inquiry_responses（対応履歴）
CREATE TABLE inquiry_responses (
    response_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    inquiry_id UUID NOT NULL REFERENCES inquiries(inquiry_id) ON DELETE CASCADE,
    responder_id UUID NOT NULL REFERENCES users(user_id),
    response_content TEXT NOT NULL,
    action_type response_action NOT NULL,
    is_internal BOOLEAN NOT NULL DEFAULT FALSE,
    attachments JSONB DEFAULT '{}',
    responded_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX idx_inquiry_responses_inquiry_id ON inquiry_responses (inquiry_id, responded_at);
CREATE INDEX idx_inquiry_responses_responder_id ON inquiry_responses (responder_id);
CREATE INDEX idx_inquiry_responses_action_type ON inquiry_responses (action_type);

-- 12. audit_logs（監査ログ）- パーティション対応
CREATE TABLE audit_logs (
    log_id UUID NOT NULL DEFAULT uuid_generate_v4(),
    table_name VARCHAR(50) NOT NULL,
    operation VARCHAR(10) NOT NULL,
    old_data JSONB,
    new_data JSONB,
    user_id UUID,
    executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (log_id, executed_at)
) PARTITION BY RANGE (executed_at);

-- 監査ログ用パーティション
CREATE TABLE audit_logs_2025_09 PARTITION OF audit_logs
FOR VALUES FROM ('2025-09-01') TO ('2025-10-01');

-- インデックス
CREATE INDEX idx_audit_logs_table_operation ON audit_logs (table_name, operation, executed_at);
CREATE INDEX idx_audit_logs_user_id ON audit_logs (user_id, executed_at DESC);

-- 監査トリガーの設定
-- ================================================================

-- 重要テーブルに監査トリガーを設定
CREATE TRIGGER audit_users 
    AFTER INSERT OR UPDATE OR DELETE ON users
    FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();

CREATE TRIGGER audit_reservations 
    AFTER INSERT OR UPDATE OR DELETE ON reservations
    FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();

CREATE TRIGGER audit_rooms 
    AFTER INSERT OR UPDATE OR DELETE ON rooms
    FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();

-- パーティション管理関数
-- ================================================================

-- 月次パーティション自動作成関数
CREATE OR REPLACE FUNCTION create_monthly_partitions()
RETURNS void AS $$
DECLARE
    start_date date;
    end_date date;
    table_suffix text;
    reservation_table text;
    history_table text;
    audit_table text;
BEGIN
    -- 次月のパーティション作成
    start_date := date_trunc('month', CURRENT_DATE + interval '1 month');
    end_date := date_trunc('month', start_date + interval '1 month');
    table_suffix := to_char(start_date, 'YYYY_MM');
    
    reservation_table := 'reservations_' || table_suffix;
    history_table := 'usage_history_' || table_suffix;
    audit_table := 'audit_logs_' || table_suffix;
    
    -- reservationsパーティション作成
    EXECUTE format('CREATE TABLE IF NOT EXISTS %I PARTITION OF reservations
                   FOR VALUES FROM (%L) TO (%L)',
                   reservation_table, start_date, end_date);
    
    -- usage_historyパーティション作成
    EXECUTE format('CREATE TABLE IF NOT EXISTS %I PARTITION OF usage_history
                   FOR VALUES FROM (%L) TO (%L)',
                   history_table, start_date, end_date);
    
    -- audit_logsパーティション作成
    EXECUTE format('CREATE TABLE IF NOT EXISTS %I PARTITION OF audit_logs
                   FOR VALUES FROM (%L) TO (%L)',
                   audit_table, start_date, end_date);
    
    RAISE NOTICE 'Created partitions for %', table_suffix;
END;
$$ LANGUAGE plpgsql;

-- ビューの作成
-- ================================================================

-- アクティブ予約ビュー
CREATE VIEW active_reservations AS
SELECT 
    r.reservation_id,
    r.user_id,
    u.full_name as user_name,
    r.room_id,
    rm.room_name,
    r.start_datetime,
    r.end_datetime,
    r.participant_count,
    r.purpose,
    r.status
FROM reservations r
JOIN users u ON r.user_id = u.user_id
JOIN rooms rm ON r.room_id = rm.room_id
WHERE r.status = 'CONFIRMED' 
    AND r.start_datetime > CURRENT_TIMESTAMP;

-- 問い合わせ検索ビュー
CREATE VIEW inquiry_search AS
SELECT 
    inquiry_id,
    inquirer_name,
    inquirer_email,
    subject,
    content,
    priority,
    status,
    received_at,
    to_tsvector('japanese', subject || ' ' || content) as search_vector
FROM inquiries;

-- 会議室利用状況ビュー
CREATE VIEW room_utilization AS
SELECT 
    r.room_id,
    r.room_name,
    r.capacity,
    r.status,
    COUNT(res.reservation_id) as total_reservations,
    COUNT(CASE WHEN res.start_datetime >= date_trunc('month', CURRENT_DATE) THEN 1 END) as current_month_reservations
FROM rooms r
LEFT JOIN reservations res ON r.room_id = res.room_id AND res.status = 'CONFIRMED'
GROUP BY r.room_id, r.room_name, r.capacity, r.status;

-- パフォーマンス監視ビュー
-- ================================================================

-- 遅いクエリ監視
CREATE VIEW slow_queries AS
SELECT 
    query,
    mean_time,
    calls,
    total_time,
    rows,
    100.0 * shared_blks_hit / nullif(shared_blks_hit + shared_blks_read, 0) AS hit_percent
FROM pg_stat_statements 
WHERE mean_time > 100
ORDER BY mean_time DESC;

-- インデックス使用状況
CREATE VIEW index_usage AS
SELECT 
    schemaname,
    tablename,
    indexname,
    idx_scan,
    idx_tup_read,
    idx_tup_fetch,
    pg_size_pretty(pg_relation_size(indexrelid)) as index_size
FROM pg_stat_user_indexes
ORDER BY idx_scan DESC;

-- 初期データ投入
-- ================================================================

-- 管理者ユーザー作成
INSERT INTO users (full_name, email, role) VALUES
('システム管理者', 'admin@example.com', 'ADMIN');

-- サンプル設備データ
INSERT INTO equipment (equipment_name, category, description) VALUES
('プロジェクター', '映像機器', 'Full HD対応プロジェクター'),
('ホワイトボード', '文具', '大型ホワイトボード'),
('テレビモニター', '映像機器', '55インチ4Kモニター'),
('マイク', '音響機器', 'ワイヤレスマイク'),
('Wi-Fi', 'ネットワーク', '高速Wi-Fi接続');

-- サンプル会議室データ
INSERT INTO rooms (room_name, capacity, location, floor_number) VALUES
('会議室A', 6, '1階東側', 1),
('会議室B', 10, '1階西側', 1),
('大会議室', 20, '2階中央', 2),
('小会議室1', 4, '2階東側', 2),
('小会議室2', 4, '2階西側', 2);

-- 権限設定
-- ================================================================

-- アプリケーションユーザーの作成
CREATE USER app_user WITH PASSWORD 'secure_password';
GRANT CONNECT ON DATABASE meeting_room_reservation_system TO app_user;
GRANT USAGE ON SCHEMA public TO app_user;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO app_user;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO app_user;

-- 読み取り専用ユーザー
CREATE USER readonly_user WITH PASSWORD 'readonly_password';
GRANT CONNECT ON DATABASE meeting_room_reservation_system TO readonly_user;
GRANT USAGE ON SCHEMA public TO readonly_user;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO readonly_user;

-- 完了メッセージ
SELECT 'データベースセットアップが完了しました。' as message;