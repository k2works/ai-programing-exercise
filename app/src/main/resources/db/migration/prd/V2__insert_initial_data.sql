-- 初期管理者ユーザーの追加
-- パスワードは "password" をBCryptでハッシュ化したもの
-- 本番環境では必ず変更すること
INSERT INTO usr (user_id, first_name, last_name, password, role_name) VALUES
('admin', '管理者', '太郎', '$2a$10$oxSJl.keBwxmsMLkcT9lPeAIxfNTPNQxpeywMrF7A3kVszwUTqfTK', 'ADMIN');

-- 会議室の追加
INSERT INTO meeting_room (room_name) VALUES
('会議室A'),
('会議室B'),
('会議室C'),
('会議室D'),
('会議室E');

-- 予約可能会議室の追加（今日から30日分）
-- PostgreSQLでは日付計算にgenerate_series関数を使用
INSERT INTO reservable_room (reserved_date, room_id)
SELECT 
    CURRENT_DATE + day_offset AS reserved_date,
    room_id
FROM 
    generate_series(0, 29) AS day_offset
CROSS JOIN 
    (SELECT room_id FROM meeting_room) AS rooms;
