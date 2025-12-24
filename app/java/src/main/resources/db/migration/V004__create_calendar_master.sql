-- 日付区分 ENUM
CREATE TYPE 日付区分 AS ENUM ('稼働日', '休日', '半日稼働');

-- カレンダマスタ
CREATE TABLE "カレンダマスタ" (
    "カレンダコード" VARCHAR(20) NOT NULL,
    "日付" DATE NOT NULL,
    "日付区分" 日付区分 NOT NULL DEFAULT '稼働日',
    "稼働時間" DECIMAL(5, 2),
    "備考" VARCHAR(200),
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("カレンダコード", "日付")
);
