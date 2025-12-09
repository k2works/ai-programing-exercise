-- 仕訳エントリテーブルを作成
CREATE TABLE "仕訳エントリ" (
    "伝票番号" VARCHAR(10) PRIMARY KEY,
    "仕訳日" DATE NOT NULL,
    "摘要" VARCHAR(100) NOT NULL,
    "合計金額" DECIMAL(15, 2) NOT NULL,
    "参照番号" VARCHAR(20),
    "作成者" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者" VARCHAR(20),
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- 仕訳明細テーブルを作成
CREATE TABLE "仕訳明細" (
    "伝票番号" VARCHAR(10) NOT NULL,
    "行番号" INTEGER NOT NULL,
    "勘定科目コード" VARCHAR(20) NOT NULL,
    "借方金額" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "貸方金額" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "摘要" VARCHAR(100) NOT NULL,
    "消費税額" DECIMAL(15, 2) DEFAULT 0,
    "消費税率" DECIMAL(5, 2),
    PRIMARY KEY ("伝票番号", "行番号")
);

-- 外部キー制約
ALTER TABLE "仕訳明細"
    ADD CONSTRAINT fk_journal_detail_entry
    FOREIGN KEY ("伝票番号")
    REFERENCES "仕訳エントリ" ("伝票番号")
    ON DELETE CASCADE;

ALTER TABLE "仕訳明細"
    ADD CONSTRAINT fk_journal_detail_account
    FOREIGN KEY ("勘定科目コード")
    REFERENCES "勘定科目マスタ" ("勘定科目コード");

-- インデックスを作成
CREATE INDEX idx_journal_entry_date
    ON "仕訳エントリ" ("仕訳日");

CREATE INDEX idx_journal_detail_account
    ON "仕訳明細" ("勘定科目コード");

-- コメント追加
COMMENT ON TABLE "仕訳エントリ" IS '仕訳エントリテーブル（ヘッダー）';
COMMENT ON COLUMN "仕訳エントリ"."伝票番号" IS '一意の伝票番号';
COMMENT ON COLUMN "仕訳エントリ"."仕訳日" IS '仕訳の日付';
COMMENT ON COLUMN "仕訳エントリ"."摘要" IS '仕訳の説明';
COMMENT ON COLUMN "仕訳エントリ"."合計金額" IS '借方・貸方の合計金額';

COMMENT ON TABLE "仕訳明細" IS '仕訳明細テーブル（明細行）';
COMMENT ON COLUMN "仕訳明細"."借方金額" IS '借方（デビット）の金額';
COMMENT ON COLUMN "仕訳明細"."貸方金額" IS '貸方（クレジット）の金額';
