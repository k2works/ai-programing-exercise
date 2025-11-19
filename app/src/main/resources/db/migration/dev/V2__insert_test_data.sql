-- テストユーザーの追加
-- パスワードは "password" をBCryptでハッシュ化したもの
INSERT INTO usr (user_id, first_name, last_name, password, role_name) VALUES
('admin', '管理者', '太郎', '$2a$10$oxSJl.keBwxmsMLkcT9lPeAIxfNTPNQxpeywMrF7A3kVszwUTqfTK', 'ADMIN'),
('user1', '一般', '花子', '$2a$10$oxSJl.keBwxmsMLkcT9lPeAIxfNTPNQxpeywMrF7A3kVszwUTqfTK', 'USER'),
('user2', '一般', '次郎', '$2a$10$oxSJl.keBwxmsMLkcT9lPeAIxfNTPNQxpeywMrF7A3kVszwUTqfTK', 'USER');

-- 会議室の追加
INSERT INTO meeting_room (room_name) VALUES
('会議室A'),
('会議室B'),
('会議室C'),
('会議室D'),
('会議室E');

-- 予約可能会議室の追加（今日から30日分）
-- H2では日付計算にDATEADD関数を使用
INSERT INTO reservable_room (reserved_date, room_id)
SELECT DATEADD('DAY', day_offset, CURRENT_DATE) AS reserved_date, room_id
FROM (
    SELECT 0 AS day_offset UNION ALL SELECT 1 UNION ALL SELECT 2 UNION ALL SELECT 3 UNION ALL SELECT 4 UNION ALL
    SELECT 5 UNION ALL SELECT 6 UNION ALL SELECT 7 UNION ALL SELECT 8 UNION ALL SELECT 9 UNION ALL
    SELECT 10 UNION ALL SELECT 11 UNION ALL SELECT 12 UNION ALL SELECT 13 UNION ALL SELECT 14 UNION ALL
    SELECT 15 UNION ALL SELECT 16 UNION ALL SELECT 17 UNION ALL SELECT 18 UNION ALL SELECT 19 UNION ALL
    SELECT 20 UNION ALL SELECT 21 UNION ALL SELECT 22 UNION ALL SELECT 23 UNION ALL SELECT 24 UNION ALL
    SELECT 25 UNION ALL SELECT 26 UNION ALL SELECT 27 UNION ALL SELECT 28 UNION ALL SELECT 29
) AS days
CROSS JOIN (
    SELECT room_id FROM meeting_room
) AS rooms;

-- サンプル予約の追加（今日の予約）
INSERT INTO reservation (start_time, end_time, reserved_date, room_id, user_id) VALUES
('09:00:00', '10:00:00', CURRENT_DATE, 1, 'user1'),
('10:30:00', '12:00:00', CURRENT_DATE, 1, 'user2'),
('13:00:00', '14:30:00', CURRENT_DATE, 2, 'admin'),
('15:00:00', '17:00:00', CURRENT_DATE, 3, 'user1');
