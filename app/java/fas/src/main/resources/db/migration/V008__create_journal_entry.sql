-- 仕訳エントリテーブルを作成
CREATE TABLE IF NOT EXISTS "仕訳エントリ" (
    "伝票番号" VARCHAR(10) PRIMARY KEY,
    "仕訳日" DATE NOT NULL,
    "摘要" VARCHAR(100) NOT NULL,
    "合計金額" DECIMAL(15,2) NOT NULL,
    "参照番号" VARCHAR(20),
    "作成者" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    "更新者" VARCHAR(20),
    "更新日時" TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

-- 仕訳明細テーブルを作成
CREATE TABLE IF NOT EXISTS "仕訳明細" (
    "伝票番号" VARCHAR(10),
    "行番号" INTEGER,
    "勘定科目コード" VARCHAR(20) NOT NULL,
    "借方金額" DECIMAL(15,2) DEFAULT 0 NOT NULL,
    "貸方金額" DECIMAL(15,2) DEFAULT 0 NOT NULL,
    "摘要" VARCHAR(100) NOT NULL,
    "消費税額" DECIMAL(15,2) DEFAULT 0,
    "消費税率" DECIMAL(5,2),
    PRIMARY KEY ("伝票番号", "行番号"),
    FOREIGN KEY ("伝票番号") REFERENCES "仕訳エントリ" ("伝票番号") ON DELETE CASCADE,
    FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ" ("勘定科目コード")
);

-- テーブルコメント
COMMENT ON TABLE "仕訳エントリ" IS
    '仕訳エントリ（複式簿記の仕訳データ）';

COMMENT ON TABLE "仕訳明細" IS
    '仕訳明細（仕訳エントリの明細行データ）';

-- カラムコメント（仕訳エントリ）
COMMENT ON COLUMN "仕訳エントリ"."伝票番号" IS '伝票番号（主キー）';
COMMENT ON COLUMN "仕訳エントリ"."仕訳日" IS '仕訳日';
COMMENT ON COLUMN "仕訳エントリ"."摘要" IS '摘要';
COMMENT ON COLUMN "仕訳エントリ"."合計金額" IS '合計金額';
COMMENT ON COLUMN "仕訳エントリ"."参照番号" IS '参照番号';
COMMENT ON COLUMN "仕訳エントリ"."作成者" IS '作成者';
COMMENT ON COLUMN "仕訳エントリ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "仕訳エントリ"."更新者" IS '更新者';
COMMENT ON COLUMN "仕訳エントリ"."更新日時" IS '更新日時';

-- カラムコメント（仕訳明細）
COMMENT ON COLUMN "仕訳明細"."伝票番号" IS '伝票番号（複合主キー1）';
COMMENT ON COLUMN "仕訳明細"."行番号" IS '行番号（複合主キー2）';
COMMENT ON COLUMN "仕訳明細"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "仕訳明細"."借方金額" IS '借方金額';
COMMENT ON COLUMN "仕訳明細"."貸方金額" IS '貸方金額';
COMMENT ON COLUMN "仕訳明細"."摘要" IS '摘要';
COMMENT ON COLUMN "仕訳明細"."消費税額" IS '消費税額';
COMMENT ON COLUMN "仕訳明細"."消費税率" IS '消費税率';

-- インデックスを作成
CREATE INDEX IF NOT EXISTS idx_journal_entry_date
    ON "仕訳エントリ" ("仕訳日");

CREATE INDEX IF NOT EXISTS idx_journal_detail_account
    ON "仕訳明細" ("勘定科目コード");
