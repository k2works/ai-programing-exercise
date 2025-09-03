-- PostgreSQL schema initialization for MRS
-- Migration from H2 to PostgreSQL

-- Users table
CREATE TABLE IF NOT EXISTS usr (
  user_id VARCHAR(50) PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  password_hash VARCHAR(200) NOT NULL,
  role VARCHAR(30) NOT NULL
);

-- Demo users (password = "demo")
-- BCrypt hash for "demo" with cost factor 10
INSERT INTO usr (user_id, name, password_hash, role)
SELECT 'user1', 'User One', '$2a$10$73qApiMRGtH8pmqQPgTfPO6JA6tfj0n3GwmyuzMwG3EfDlzuHJ/7C', 'USER'
WHERE NOT EXISTS (SELECT 1 FROM usr WHERE user_id = 'user1');

-- Demo users for frontend (password = "demo")  
INSERT INTO usr (user_id, name, password_hash, role)
SELECT 'taro', 'Taro Yamada', '$2a$10$73qApiMRGtH8pmqQPgTfPO6JA6tfj0n3GwmyuzMwG3EfDlzuHJ/7C', 'USER'
WHERE NOT EXISTS (SELECT 1 FROM usr WHERE user_id = 'taro');

INSERT INTO usr (user_id, name, password_hash, role)
SELECT 'hanako', 'Hanako Sato', '$2a$10$73qApiMRGtH8pmqQPgTfPO6JA6tfj0n3GwmyuzMwG3EfDlzuHJ/7C', 'USER'
WHERE NOT EXISTS (SELECT 1 FROM usr WHERE user_id = 'hanako');

INSERT INTO usr (user_id, name, password_hash, role)
SELECT 'admin1', 'Admin User', '$2a$10$73qApiMRGtH8pmqQPgTfPO6JA6tfj0n3GwmyuzMwG3EfDlzuHJ/7C', 'ADMIN'
WHERE NOT EXISTS (SELECT 1 FROM usr WHERE user_id = 'admin1');

-- Meeting rooms table
CREATE TABLE IF NOT EXISTS meeting_room (
  room_id INTEGER PRIMARY KEY,
  room_name VARCHAR(100) NOT NULL
);

-- Reservable rooms table (with optimistic locking support)
CREATE TABLE IF NOT EXISTS reservable_room (
  room_id INTEGER NOT NULL,
  reservable_date DATE NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (room_id, reservable_date),
  FOREIGN KEY (room_id) REFERENCES meeting_room(room_id) ON DELETE CASCADE
);

-- Reservations table (with PostgreSQL SERIAL for auto-increment)
CREATE TABLE IF NOT EXISTS reservation (
  reservation_id SERIAL PRIMARY KEY,
  start_time TIME NOT NULL,
  end_time TIME NOT NULL,
  room_id INTEGER NOT NULL,
  reserved_date DATE NOT NULL,
  user_id VARCHAR(50) NOT NULL,
  created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
  FOREIGN KEY (room_id, reserved_date) REFERENCES reservable_room(room_id, reservable_date) ON DELETE CASCADE,
  FOREIGN KEY (user_id) REFERENCES usr(user_id) ON DELETE CASCADE
);

-- Indexes for performance optimization
CREATE INDEX IF NOT EXISTS idx_reservation_user_date ON reservation(user_id, reserved_date);
CREATE INDEX IF NOT EXISTS idx_reservation_room_date ON reservation(room_id, reserved_date);
CREATE INDEX IF NOT EXISTS idx_reservation_time_range ON reservation(reserved_date, start_time, end_time);

-- Sample meeting rooms
INSERT INTO meeting_room (room_id, room_name)
SELECT 1, 'A会議室' WHERE NOT EXISTS (SELECT 1 FROM meeting_room WHERE room_id = 1);

INSERT INTO meeting_room (room_id, room_name)
SELECT 2, 'B会議室' WHERE NOT EXISTS (SELECT 1 FROM meeting_room WHERE room_id = 2);

-- Reservable rooms for today and +/-1 day (PostgreSQL syntax)
INSERT INTO reservable_room (room_id, reservable_date)
SELECT 1, CURRENT_DATE WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=1 AND reservable_date=CURRENT_DATE);

INSERT INTO reservable_room (room_id, reservable_date)
SELECT 2, CURRENT_DATE WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=2 AND reservable_date=CURRENT_DATE);

INSERT INTO reservable_room (room_id, reservable_date)
SELECT 1, CURRENT_DATE + INTERVAL '1 day' WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=1 AND reservable_date=CURRENT_DATE + INTERVAL '1 day');

INSERT INTO reservable_room (room_id, reservable_date)
SELECT 2, CURRENT_DATE - INTERVAL '1 day' WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=2 AND reservable_date=CURRENT_DATE - INTERVAL '1 day');

-- Additional days for testing (next week)
INSERT INTO reservable_room (room_id, reservable_date)
SELECT 1, CURRENT_DATE + INTERVAL '2 days' WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=1 AND reservable_date=CURRENT_DATE + INTERVAL '2 days');

INSERT INTO reservable_room (room_id, reservable_date)
SELECT 2, CURRENT_DATE + INTERVAL '2 days' WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=2 AND reservable_date=CURRENT_DATE + INTERVAL '2 days');