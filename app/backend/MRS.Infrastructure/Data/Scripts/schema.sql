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

-- Reservations Table (for future implementation)
-- CREATE TABLE IF NOT EXISTS Reservations (
--     ReservationId VARCHAR(50) NOT NULL PRIMARY KEY,
--     ReservableRoomId VARCHAR(50) NOT NULL,
--     UserId VARCHAR(50) NOT NULL,
--     StartTime TIMESTAMP NOT NULL,
--     EndTime TIMESTAMP NOT NULL,
--     CreatedAt TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
--     FOREIGN KEY (ReservableRoomId) REFERENCES ReservableRooms(ReservableRoomId) ON DELETE RESTRICT,
--     FOREIGN KEY (UserId) REFERENCES Users(UserId) ON DELETE RESTRICT
-- );

-- Indexes for better performance
CREATE INDEX IF NOT EXISTS idx_users_name ON Users(Name);
CREATE INDEX IF NOT EXISTS idx_reservablerooms_roomid ON ReservableRooms(RoomId);
-- CREATE INDEX IF NOT EXISTS idx_reservations_reservableroomid ON Reservations(ReservableRoomId);
-- CREATE INDEX IF NOT EXISTS idx_reservations_userid ON Reservations(UserId);
-- CREATE INDEX IF NOT EXISTS idx_reservations_time ON Reservations(ReservableRoomId, StartTime, EndTime);
