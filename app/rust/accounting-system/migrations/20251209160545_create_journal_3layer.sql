-- 古い2層構造のテーブルを削除（2.1節で作成したテーブル）
DROP TABLE IF EXISTS "仕訳明細" CASCADE;
DROP TABLE IF EXISTS "仕訳エントリ" CASCADE;

-- 1. 仕訳テーブル（ヘッダー）
CREATE TABLE "仕訳" (
    "仕訳伝票番号" VARCHAR(20) PRIMARY KEY,
    "起票日" DATE NOT NULL,
    "入力日" DATE NOT NULL,
    "決算仕訳フラグ" INTEGER NOT NULL DEFAULT 0,
    "単振フラグ" INTEGER NOT NULL DEFAULT 0,
    "仕訳伝票区分" INTEGER NOT NULL DEFAULT 0,
    "定期計上フラグ" INTEGER NOT NULL DEFAULT 0,
    "社員コード" VARCHAR(10),
    "部門コード" VARCHAR(5),
    "赤伝フラグ" INTEGER NOT NULL DEFAULT 0,
    "赤黒伝票番号" VARCHAR(20),
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);

-- インデックス作成
CREATE INDEX "idx_仕訳_起票日" ON "仕訳"("起票日");
CREATE INDEX "idx_仕訳_部門コード" ON "仕訳"("部門コード");
CREATE INDEX "idx_仕訳_赤伝フラグ" ON "仕訳"("赤伝フラグ");

-- 2. 仕訳明細テーブル
CREATE TABLE "仕訳明細" (
    "仕訳伝票番号" VARCHAR(20) NOT NULL,
    "仕訳行番号" INTEGER NOT NULL,
    "行摘要" VARCHAR(1000) NOT NULL,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("仕訳伝票番号", "仕訳行番号")
);

CREATE INDEX "idx_仕訳明細_伝票番号" ON "仕訳明細"("仕訳伝票番号");

ALTER TABLE "仕訳明細"
    ADD CONSTRAINT "fk_仕訳明細_仕訳"
    FOREIGN KEY ("仕訳伝票番号")
    REFERENCES "仕訳"("仕訳伝票番号")
    ON DELETE CASCADE;

-- 3. 仕訳貸借明細テーブル
CREATE TABLE "仕訳貸借明細" (
    "仕訳伝票番号" VARCHAR(20) NOT NULL,
    "仕訳行番号" INTEGER NOT NULL,
    "仕訳行貸借区分" CHAR(1) NOT NULL,
    "通貨コード" VARCHAR(3) NOT NULL DEFAULT 'JPY',
    "為替レート" DECIMAL(10, 4) NOT NULL DEFAULT 1.0000,
    "部門コード" VARCHAR(5),
    "プロジェクトコード" VARCHAR(10),
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "補助科目コード" VARCHAR(10),
    "仕訳金額" DECIMAL(15, 2) NOT NULL,
    "基軸換算仕訳金額" DECIMAL(15, 2) NOT NULL,
    "消費税区分" VARCHAR(2),
    "消費税率" INTEGER,
    "消費税計算区分" VARCHAR(2),
    "期日" DATE,
    "資金繰フラグ" INTEGER NOT NULL DEFAULT 0,
    "セグメントコード" VARCHAR(10),
    "相手勘定科目コード" VARCHAR(10),
    "相手補助科目コード" VARCHAR(10),
    "付箋コード" VARCHAR(1),
    "付箋内容" VARCHAR(60),
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分")
);

CREATE INDEX "idx_仕訳貸借明細_伝票番号" ON "仕訳貸借明細"("仕訳伝票番号");
CREATE INDEX "idx_仕訳貸借明細_勘定科目" ON "仕訳貸借明細"("勘定科目コード");
CREATE INDEX "idx_仕訳貸借明細_部門" ON "仕訳貸借明細"("部門コード");
CREATE INDEX "idx_仕訳貸借明細_プロジェクト" ON "仕訳貸借明細"("プロジェクトコード");

ALTER TABLE "仕訳貸借明細"
    ADD CONSTRAINT "fk_仕訳貸借明細_仕訳明細"
    FOREIGN KEY ("仕訳伝票番号", "仕訳行番号")
    REFERENCES "仕訳明細"("仕訳伝票番号", "仕訳行番号")
    ON DELETE CASCADE;

ALTER TABLE "仕訳貸借明細"
    ADD CONSTRAINT "fk_仕訳貸借明細_勘定科目マスタ"
    FOREIGN KEY ("勘定科目コード")
    REFERENCES "勘定科目マスタ"("勘定科目コード");

-- CHECK制約
ALTER TABLE "仕訳貸借明細"
    ADD CONSTRAINT "check_貸借区分"
    CHECK ("仕訳行貸借区分" IN ('D', 'C'));

ALTER TABLE "仕訳貸借明細"
    ADD CONSTRAINT "check_仕訳金額"
    CHECK ("仕訳金額" >= 0);

ALTER TABLE "仕訳貸借明細"
    ADD CONSTRAINT "check_為替レート"
    CHECK ("為替レート" > 0);

ALTER TABLE "仕訳貸借明細"
    ADD CONSTRAINT "check_通貨コード長"
    CHECK (LENGTH("通貨コード") = 3);

-- テーブルコメント
COMMENT ON TABLE "仕訳" IS '仕訳ヘッダー（伝票単位の基本情報）';
COMMENT ON TABLE "仕訳明細" IS '仕訳明細（行単位の情報）';
COMMENT ON TABLE "仕訳貸借明細" IS '仕訳貸借明細（借方・貸方の詳細情報）';

-- カラムコメント
COMMENT ON COLUMN "仕訳"."起票日" IS '実際の取引発生日';
COMMENT ON COLUMN "仕訳"."入力日" IS 'システムへの入力日';
COMMENT ON COLUMN "仕訳"."決算仕訳フラグ" IS '0=通常仕訳、1=決算仕訳';
COMMENT ON COLUMN "仕訳"."単振フラグ" IS '0=複合仕訳、1=単一仕訳';
COMMENT ON COLUMN "仕訳"."赤伝フラグ" IS '0=通常、1=赤伝票（取消仕訳）';
COMMENT ON COLUMN "仕訳貸借明細"."仕訳行貸借区分" IS 'D=借方（Debit）、C=貸方（Credit）';
