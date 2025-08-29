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
