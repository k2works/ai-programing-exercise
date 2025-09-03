-- User Registration Management Table for MRS
-- ユーザー自己登録・管理者承認フロー用テーブル

-- ユーザー登録申請テーブル
CREATE TABLE IF NOT EXISTS user_registration (
  registration_id SERIAL PRIMARY KEY,
  user_id VARCHAR(50) NOT NULL UNIQUE,
  name VARCHAR(100) NOT NULL,
  email VARCHAR(255) NOT NULL,
  password_hash VARCHAR(200) NOT NULL,
  status VARCHAR(20) NOT NULL DEFAULT 'PENDING_APPROVAL',
  registered_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  approved_at TIMESTAMP,
  approved_by VARCHAR(50),
  rejected_at TIMESTAMP,
  rejected_by VARCHAR(50),
  rejection_reason TEXT,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  CONSTRAINT fk_approved_by FOREIGN KEY (approved_by) REFERENCES usr(user_id),
  CONSTRAINT fk_rejected_by FOREIGN KEY (rejected_by) REFERENCES usr(user_id),
  CONSTRAINT check_status CHECK (status IN ('PENDING_APPROVAL', 'APPROVED', 'REJECTED'))
);

-- インデックス作成
CREATE INDEX IF NOT EXISTS idx_user_registration_status ON user_registration(status);
CREATE INDEX IF NOT EXISTS idx_user_registration_registered_at ON user_registration(registered_at);
CREATE INDEX IF NOT EXISTS idx_user_registration_email ON user_registration(email);

-- usrテーブルにemailカラム追加（既存ユーザー用）
ALTER TABLE usr ADD COLUMN IF NOT EXISTS email VARCHAR(255);
ALTER TABLE usr ADD COLUMN IF NOT EXISTS created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;
ALTER TABLE usr ADD COLUMN IF NOT EXISTS updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP;

-- 既存ユーザーのデフォルトメール設定
UPDATE usr SET email = user_id || '@example.local' WHERE email IS NULL;

-- テスト用：承認済みサンプル登録申請データ
INSERT INTO user_registration (user_id, name, email, password_hash, status, approved_at, approved_by)
SELECT 'test_user', 'Test User', 'test.user@example.com', 
       '$2a$10$73qApiMRGtH8pmqQPgTfPO6JA6tfj0n3GwmyuzMwG3EfDlzuHJ/7C', 
       'APPROVED', CURRENT_TIMESTAMP, 'admin1'
WHERE NOT EXISTS (SELECT 1 FROM user_registration WHERE user_id = 'test_user');

-- テスト用：承認待ちサンプル登録申請データ
INSERT INTO user_registration (user_id, name, email, password_hash, status)
SELECT 'pending_user', 'Pending User', 'pending.user@example.com',
       '$2a$10$73qApiMRGtH8pmqQPgTfPO6JA6tfj0n3GwmyuzMwG3EfDlzuHJ/7C',
       'PENDING_APPROVAL'
WHERE NOT EXISTS (SELECT 1 FROM user_registration WHERE user_id = 'pending_user');