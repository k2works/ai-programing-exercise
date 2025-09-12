-- Meeting Room System 初期スキーマ
-- V1__Initial_schema.sql

-- Users テーブル
CREATE TABLE users (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    email VARCHAR(255) NOT NULL UNIQUE,
    name VARCHAR(255) NOT NULL,
    status VARCHAR(50) NOT NULL DEFAULT 'ACTIVE',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- Rooms テーブル
CREATE TABLE rooms (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(255) NOT NULL,
    capacity INTEGER NOT NULL,
    status VARCHAR(50) NOT NULL DEFAULT 'AVAILABLE',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
);

-- Reservations テーブル
CREATE TABLE reservations (
    id BIGINT PRIMARY KEY AUTO_INCREMENT,
    user_id BIGINT NOT NULL,
    room_id BIGINT NOT NULL,
    start_time TIMESTAMP NOT NULL,
    end_time TIMESTAMP NOT NULL,
    purpose VARCHAR(1000),
    status VARCHAR(50) NOT NULL DEFAULT 'CONFIRMED',
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (room_id) REFERENCES rooms(id) ON DELETE CASCADE
);

-- インデックス作成
CREATE INDEX idx_reservation_user_created ON reservations(user_id, created_at DESC);
CREATE INDEX idx_reservation_room_datetime ON reservations(room_id, start_time);
CREATE INDEX idx_reservation_status ON reservations(status);
CREATE INDEX idx_users_email ON users(email);
CREATE INDEX idx_rooms_status ON rooms(status);

-- サンプルデータ
INSERT INTO users (email, name, status) VALUES 
('admin@example.com', 'System Administrator', 'ACTIVE'),
('user1@example.com', 'Sample User 1', 'ACTIVE'),
('user2@example.com', 'Sample User 2', 'ACTIVE');

INSERT INTO rooms (name, capacity, status) VALUES 
('会議室A', 10, 'AVAILABLE'),
('会議室B', 6, 'AVAILABLE'),
('会議室C', 20, 'AVAILABLE');