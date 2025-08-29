-- Dev schema init for MRS

CREATE TABLE IF NOT EXISTS usr (
  user_id VARCHAR(50) PRIMARY KEY,
  name VARCHAR(100) NOT NULL,
  password_hash VARCHAR(200) NOT NULL,
  role VARCHAR(30) NOT NULL
);

-- demo users (password = "demo")
INSERT INTO usr (user_id, name, password_hash, role)
SELECT 'user1', 'User One', '$2a$10$2S7x7CN/4c0iI6pV5f5m7.7uM7xqk5h1M3E1a1cT0l2E0q9Jx9n6a', 'USER'
WHERE NOT EXISTS (SELECT 1 FROM usr WHERE user_id = 'user1');

CREATE TABLE IF NOT EXISTS meeting_room (
  room_id INT PRIMARY KEY,
  room_name VARCHAR(100) NOT NULL
);

CREATE TABLE IF NOT EXISTS reservable_room (
  room_id INT NOT NULL,
  reservable_date DATE NOT NULL,
  PRIMARY KEY (room_id, reservable_date),
  FOREIGN KEY (room_id) REFERENCES meeting_room(room_id)
);

-- sample rooms
INSERT INTO meeting_room (room_id, room_name)
SELECT 1, 'A会議室' WHERE NOT EXISTS (SELECT 1 FROM meeting_room WHERE room_id = 1);
INSERT INTO meeting_room (room_id, room_name)
SELECT 2, 'B会議室' WHERE NOT EXISTS (SELECT 1 FROM meeting_room WHERE room_id = 2);

-- today and +/-1 day
INSERT INTO reservable_room (room_id, reservable_date)
SELECT 1, CURRENT_DATE() WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=1 AND reservable_date=CURRENT_DATE());
INSERT INTO reservable_room (room_id, reservable_date)
SELECT 2, CURRENT_DATE() WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=2 AND reservable_date=CURRENT_DATE());
INSERT INTO reservable_room (room_id, reservable_date)
SELECT 1, DATEADD('DAY',1,CURRENT_DATE()) WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=1 AND reservable_date=DATEADD('DAY',1,CURRENT_DATE()));
INSERT INTO reservable_room (room_id, reservable_date)
SELECT 2, DATEADD('DAY',-1,CURRENT_DATE()) WHERE NOT EXISTS (SELECT 1 FROM reservable_room WHERE room_id=2 AND reservable_date=DATEADD('DAY',-1,CURRENT_DATE()));
