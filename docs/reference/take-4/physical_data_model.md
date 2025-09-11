# 物理データモデル - 会議室予約システム

## 概要

PostgreSQL 15を対象とした物理データモデルです。パフォーマンス、スケーラビリティ、保守性を考慮した実装仕様を定義します。

## データベース設計方針

### 基本設計原則
- **UUID**: 主キーにUUIDを使用し分散環境対応
- **JSONB**: 拡張属性にJSONB型を活用
- **ENUM**: 固定値リストにENUM型を使用
- **パーティション**: 大量データテーブルの分割
- **インデックス**: クエリパフォーマンス最適化

### データベース構成
```sql
-- データベース作成
CREATE DATABASE meeting_room_reservation_system
    ENCODING = 'UTF8'
    LC_COLLATE = 'ja_JP.UTF-8'  
    LC_CTYPE = 'ja_JP.UTF-8'
    TEMPLATE = template0;

-- 拡張機能有効化
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
CREATE EXTENSION IF NOT EXISTS "pgcrypto";
```

## テーブル定義

### 1. users（ユーザー情報）

```sql
CREATE TYPE user_role AS ENUM ('MEMBER', 'STAFF', 'ADMIN');
CREATE TYPE user_status AS ENUM ('ACTIVE', 'INACTIVE', 'SUSPENDED');

CREATE TABLE users (
    user_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    full_name VARCHAR(100) NOT NULL,
    email VARCHAR(255) NOT NULL,
    phone_number VARCHAR(20),
    organization VARCHAR(100),
    role user_role NOT NULL DEFAULT 'MEMBER',
    status user_status NOT NULL DEFAULT 'ACTIVE',
    preferences JSONB,
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

-- トリガー（更新日時自動更新）
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    NEW.version = OLD.version + 1;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER trigger_users_updated_at
    BEFORE UPDATE ON users
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();
```

### 2. login_info（ログイン情報）

```sql
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
```

### 3. rooms（会議室情報）

```sql
CREATE TYPE room_status AS ENUM ('AVAILABLE', 'MAINTENANCE', 'DISABLED');

CREATE TABLE rooms (
    room_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    room_name VARCHAR(100) NOT NULL,
    capacity INTEGER NOT NULL,
    location VARCHAR(100) NOT NULL,
    description TEXT,
    status room_status NOT NULL DEFAULT 'AVAILABLE',
    floor_number INTEGER,
    attributes JSONB,
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
```

### 4. equipment（設備情報）

```sql
CREATE TYPE equipment_status AS ENUM ('AVAILABLE', 'DISABLED');

CREATE TABLE equipment (
    equipment_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    equipment_name VARCHAR(50) NOT NULL,
    category VARCHAR(30) NOT NULL,
    description TEXT,
    status equipment_status NOT NULL DEFAULT 'AVAILABLE',
    specifications JSONB,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE UNIQUE INDEX idx_equipment_name_category ON equipment (equipment_name, category);
CREATE INDEX idx_equipment_status ON equipment (status);
CREATE INDEX idx_equipment_category ON equipment (category);
```

### 5. room_equipment（会議室設備関連）

```sql
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
```

### 6. reservations（予約情報）- パーティション対応

```sql
CREATE TYPE reservation_status AS ENUM ('CONFIRMED', 'CANCELLED', 'FORCE_CANCELLED', 'COMPLETED');

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
    metadata JSONB,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    version INTEGER NOT NULL DEFAULT 1,
    PRIMARY KEY (reservation_id, start_datetime)
) PARTITION BY RANGE (start_datetime);

-- 月次パーティション作成
CREATE TABLE reservations_2025_01 PARTITION OF reservations
FOR VALUES FROM ('2025-01-01') TO ('2025-02-01');

CREATE TABLE reservations_2025_02 PARTITION OF reservations
FOR VALUES FROM ('2025-02-01') TO ('2025-03-01');

-- インデックス
CREATE INDEX idx_reservations_user_start_desc ON reservations (user_id, start_datetime DESC, status);
CREATE INDEX idx_reservations_room_start ON reservations (room_id, start_datetime, end_datetime, status);
CREATE INDEX idx_reservations_status_start ON reservations (status, start_datetime);
CREATE INDEX idx_reservations_metadata_gin ON reservations USING GIN (metadata);

-- 一意制約（同時間帯重複防止）
CREATE UNIQUE INDEX idx_reservations_room_time_overlap ON reservations 
(room_id, start_datetime, end_datetime) 
WHERE status IN ('CONFIRMED');

-- 制約
ALTER TABLE reservations 
ADD CONSTRAINT chk_reservations_time_order 
CHECK (end_datetime > start_datetime);

ALTER TABLE reservations 
ADD CONSTRAINT chk_reservations_participant_count 
CHECK (participant_count > 0);
```

### 7. cancellation_info（キャンセル情報）

```sql
CREATE TABLE cancellation_info (
    cancellation_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    reservation_id UUID NOT NULL,
    cancelled_by UUID NOT NULL REFERENCES users(user_id),
    reason TEXT,
    cancellation_type VARCHAR(20) NOT NULL DEFAULT 'USER',
    cancelled_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    metadata JSONB
);

-- インデックス
CREATE INDEX idx_cancellation_info_reservation_id ON cancellation_info (reservation_id);
CREATE INDEX idx_cancellation_info_cancelled_by ON cancellation_info (cancelled_by);
CREATE INDEX idx_cancellation_info_cancelled_at ON cancellation_info (cancelled_at DESC);
```

### 8. maintenance（メンテナンス情報）

```sql
CREATE TYPE maintenance_status AS ENUM ('SCHEDULED', 'IN_PROGRESS', 'COMPLETED', 'CANCELLED');

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
```

### 9. usage_history（利用履歴）- パーティション対応

```sql
CREATE TYPE usage_action AS ENUM ('CREATE', 'UPDATE', 'CANCEL', 'FORCE_CANCEL', 'COMPLETE');

CREATE TABLE usage_history (
    history_id UUID NOT NULL DEFAULT uuid_generate_v4(),
    user_id UUID NOT NULL REFERENCES users(user_id),
    reservation_id UUID,
    action usage_action NOT NULL,
    details JSONB,
    ip_address INET,
    user_agent TEXT,
    executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (history_id, executed_at)
) PARTITION BY RANGE (executed_at);

-- 月次パーティション（履歴データ）
CREATE TABLE usage_history_2025_01 PARTITION OF usage_history
FOR VALUES FROM ('2025-01-01') TO ('2025-02-01');

-- インデックス
CREATE INDEX idx_usage_history_user_executed_desc ON usage_history (user_id, executed_at DESC);
CREATE INDEX idx_usage_history_reservation_id ON usage_history (reservation_id);
CREATE INDEX idx_usage_history_action_executed ON usage_history (action, executed_at);
CREATE INDEX idx_usage_history_details_gin ON usage_history USING GIN (details);
```

### 10. inquiries（問い合わせ情報）

```sql
CREATE TYPE inquiry_priority AS ENUM ('LOW', 'MEDIUM', 'HIGH', 'URGENT');
CREATE TYPE inquiry_status AS ENUM ('RECEIVED', 'IN_PROGRESS', 'RESOLVED', 'CLOSED');

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
    metadata JSONB,
    received_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX idx_inquiries_status_priority ON inquiries (status, priority, received_at);
CREATE INDEX idx_inquiries_assigned_to ON inquiries (assigned_to);
CREATE INDEX idx_inquiries_email ON inquiries (inquirer_email);
CREATE INDEX idx_inquiries_received_at ON inquiries (received_at DESC);
```

### 11. inquiry_responses（対応履歴）

```sql
CREATE TYPE response_action AS ENUM ('ACKNOWLEDGED', 'REPLIED', 'INVESTIGATED', 'COMPLETED', 'ESCALATED');

CREATE TABLE inquiry_responses (
    response_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    inquiry_id UUID NOT NULL REFERENCES inquiries(inquiry_id) ON DELETE CASCADE,
    responder_id UUID NOT NULL REFERENCES users(user_id),
    response_content TEXT NOT NULL,
    action_type response_action NOT NULL,
    is_internal BOOLEAN NOT NULL DEFAULT FALSE,
    attachments JSONB,
    responded_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス
CREATE INDEX idx_inquiry_responses_inquiry_id ON inquiry_responses (inquiry_id, responded_at);
CREATE INDEX idx_inquiry_responses_responder_id ON inquiry_responses (responder_id);
CREATE INDEX idx_inquiry_responses_action_type ON inquiry_responses (action_type);
```

## 高度な機能

### 1. 全文検索

```sql
-- 全文検索用インデックス
CREATE INDEX idx_inquiries_content_fts ON inquiries 
USING GIN (to_tsvector('japanese', subject || ' ' || content));

-- 検索用ビュー
CREATE VIEW inquiry_search AS
SELECT 
    inquiry_id,
    inquirer_name,
    subject,
    content,
    to_tsvector('japanese', subject || ' ' || content) as search_vector
FROM inquiries;
```

### 2. 監査ログ

```sql
CREATE TABLE audit_logs (
    log_id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    table_name VARCHAR(50) NOT NULL,
    operation VARCHAR(10) NOT NULL,
    old_data JSONB,
    new_data JSONB,
    user_id UUID,
    executed_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
) PARTITION BY RANGE (executed_at);

-- 監査ログトリガー関数
CREATE OR REPLACE FUNCTION audit_trigger_function()
RETURNS TRIGGER AS $$
BEGIN
    IF TG_OP = 'INSERT' THEN
        INSERT INTO audit_logs (table_name, operation, new_data, user_id)
        VALUES (TG_TABLE_NAME, TG_OP, row_to_json(NEW), current_setting('app.current_user_id', true)::UUID);
        RETURN NEW;
    ELSIF TG_OP = 'UPDATE' THEN
        INSERT INTO audit_logs (table_name, operation, old_data, new_data, user_id)
        VALUES (TG_TABLE_NAME, TG_OP, row_to_json(OLD), row_to_json(NEW), current_setting('app.current_user_id', true)::UUID);
        RETURN NEW;
    ELSIF TG_OP = 'DELETE' THEN
        INSERT INTO audit_logs (table_name, operation, old_data, user_id)
        VALUES (TG_TABLE_NAME, TG_OP, row_to_json(OLD), current_setting('app.current_user_id', true)::UUID);
        RETURN OLD;
    END IF;
    RETURN NULL;
END;
$$ language 'plpgsql';

-- 監査対象テーブルにトリガー適用
CREATE TRIGGER audit_users AFTER INSERT OR UPDATE OR DELETE ON users
    FOR EACH ROW EXECUTE FUNCTION audit_trigger_function();
```

### 3. パフォーマンス最適化

```sql
-- 統計情報の自動更新
ALTER TABLE reservations SET (autovacuum_analyze_scale_factor = 0.02);
ALTER TABLE usage_history SET (autovacuum_analyze_scale_factor = 0.01);

-- 部分インデックス（アクティブデータのみ）
CREATE INDEX idx_reservations_active ON reservations (room_id, start_datetime)
WHERE status IN ('CONFIRMED') AND start_datetime > CURRENT_TIMESTAMP;

-- 式インデックス
CREATE INDEX idx_reservations_duration ON reservations 
((end_datetime - start_datetime));
```

## データベース運用

### バックアップ戦略

```bash
# 日次完全バックアップ
pg_dump -h localhost -U postgres -d meeting_room_reservation_system \
    --format=custom --compress=9 --verbose \
    --file=backup_$(date +%Y%m%d).dump

# 継続的アーカイブログ
archive_mode = on
archive_command = 'cp %p /var/lib/postgresql/archive/%f'
```

### パーティション管理

```sql
-- 月次パーティション自動作成関数
CREATE OR REPLACE FUNCTION create_monthly_partitions()
RETURNS void AS $$
DECLARE
    start_date date;
    end_date date;
    table_name text;
BEGIN
    start_date := date_trunc('month', CURRENT_DATE + interval '1 month');
    end_date := date_trunc('month', start_date + interval '1 month');
    
    table_name := 'reservations_' || to_char(start_date, 'YYYY_MM');
    
    EXECUTE format('CREATE TABLE IF NOT EXISTS %I PARTITION OF reservations
                   FOR VALUES FROM (%L) TO (%L)',
                   table_name, start_date, end_date);
END;
$$ LANGUAGE plpgsql;

-- 月次実行スケジュール
SELECT cron.schedule('create_partitions', '0 0 1 * *', 'SELECT create_monthly_partitions();');
```

---

## パフォーマンス指標

### 目標値
- **検索レスポンス**: 1秒以内
- **予約作成**: 2秒以内
- **同時接続**: 100ユーザー
- **データ保持**: 7年間

### モニタリング

```sql
-- 遅いクエリの監視
CREATE VIEW slow_queries AS
SELECT 
    query,
    mean_time,
    calls,
    total_time
FROM pg_stat_statements 
WHERE mean_time > 1000
ORDER BY mean_time DESC;

-- インデックス使用状況
CREATE VIEW index_usage AS
SELECT 
    schemaname,
    tablename,
    indexname,
    idx_scan,
    idx_tup_read,
    idx_tup_fetch
FROM pg_stat_user_indexes
ORDER BY idx_scan DESC;
```

---

## 最終更新
- **作成日**: 2025-09-10
- **作成者**: AI Assistant
- **バージョン**: 1.0.0