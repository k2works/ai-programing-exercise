-- MRS (Meeting Room System) Database Schema
-- PostgreSQL Version

-- Users Table
CREATE TABLE IF NOT EXISTS Users (
    UserId VARCHAR(50) NOT NULL PRIMARY KEY,
    Name VARCHAR(100) NOT NULL UNIQUE,
    HashedPassword VARCHAR(255) NOT NULL,
    Role VARCHAR(50) NOT NULL,
    IsActive BOOLEAN NOT NULL DEFAULT true,
    CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- Rooms Table  
CREATE TABLE IF NOT EXISTS Rooms (
    RoomId VARCHAR(50) NOT NULL PRIMARY KEY,
    RoomName VARCHAR(100) NOT NULL,
    Capacity INTEGER NOT NULL,
    IsActive BOOLEAN NOT NULL DEFAULT true,
    CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- ReservableRooms Table
CREATE TABLE IF NOT EXISTS ReservableRooms (
    ReservableRoomId VARCHAR(50) NOT NULL PRIMARY KEY,
    RoomId VARCHAR(50) NOT NULL,
    RoomName VARCHAR(100) NOT NULL,
    IsAvailable BOOLEAN NOT NULL DEFAULT true,
    CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (RoomId) REFERENCES Rooms(RoomId) ON DELETE RESTRICT
);

-- Reservations Table
CREATE TABLE IF NOT EXISTS Reservations (
    ReservationId VARCHAR(50) NOT NULL PRIMARY KEY,
    RoomId VARCHAR(50) NOT NULL,
    UserId VARCHAR(50) NOT NULL,
    Title VARCHAR(200) NOT NULL,
    StartTime TIMESTAMP NOT NULL,
    EndTime TIMESTAMP NOT NULL,
    Participants TEXT,  -- JSON配列として格納
    Status VARCHAR(20) DEFAULT 'confirmed',
    RowVersion INTEGER DEFAULT 0,  -- 楽観的ロック用
    CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    UpdatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (RoomId) REFERENCES Rooms(RoomId) ON DELETE RESTRICT,
    FOREIGN KEY (UserId) REFERENCES Users(UserId) ON DELETE RESTRICT
);

-- Indexes for better performance
CREATE INDEX IF NOT EXISTS idx_users_name ON Users(Name);
CREATE INDEX IF NOT EXISTS idx_reservablerooms_roomid ON ReservableRooms(RoomId);

-- 予約テーブルのインデックス
CREATE INDEX IF NOT EXISTS idx_reservations_room_time ON Reservations(RoomId, StartTime, EndTime);
CREATE INDEX IF NOT EXISTS idx_reservations_user ON Reservations(UserId);
CREATE INDEX IF NOT EXISTS idx_reservations_date ON Reservations(DATE(StartTime));

-- 重複予約防止制約（同一会議室・時間帯）
CREATE UNIQUE INDEX IF NOT EXISTS idx_no_overlapping_reservations 
ON Reservations(RoomId, StartTime, EndTime) 
WHERE Status = 'confirmed';
