-- 月次勘定科目残高テーブル
CREATE TABLE "月次勘定科目残高" (
    "決算期" INTEGER NOT NULL,
    "月度" INTEGER NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "補助科目コード" VARCHAR(10) NOT NULL DEFAULT '',
    "部門コード" VARCHAR(5) NOT NULL DEFAULT '',
    "プロジェクトコード" VARCHAR(10) NOT NULL DEFAULT '',
    "決算仕訳フラグ" INTEGER NOT NULL DEFAULT 0,
    "月初残高" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "借方金額" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "貸方金額" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "月末残高" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
);

-- 外部キー制約
ALTER TABLE "月次勘定科目残高"
    ADD CONSTRAINT "fk_月次勘定科目残高_勘定科目マスタ"
    FOREIGN KEY ("勘定科目コード")
    REFERENCES "勘定科目マスタ"("勘定科目コード");

-- インデックス作成
CREATE INDEX "idx_月次勘定科目残高_決算期月度" ON "月次勘定科目残高"("決算期", "月度");
CREATE INDEX "idx_月次勘定科目残高_勘定科目" ON "月次勘定科目残高"("勘定科目コード");
CREATE INDEX "idx_月次勘定科目残高_部門" ON "月次勘定科目残高"("部門コード");
CREATE INDEX "idx_月次勘定科目残高_プロジェクト" ON "月次勘定科目残高"("プロジェクトコード");

-- CHECK 制約
ALTER TABLE "月次勘定科目残高"
    ADD CONSTRAINT "check_月次残高_月度"
    CHECK ("月度" BETWEEN 1 AND 12);

ALTER TABLE "月次勘定科目残高"
    ADD CONSTRAINT "check_月次残高_決算仕訳フラグ"
    CHECK ("決算仕訳フラグ" IN (0, 1));

-- コメント追加
COMMENT ON TABLE "月次勘定科目残高" IS '月次勘定科目残高（月ごとの月初残高・借方金額・貸方金額・月末残高を記録）';
COMMENT ON COLUMN "月次勘定科目残高"."決算期" IS '会計年度（例：2025）';
COMMENT ON COLUMN "月次勘定科目残高"."月度" IS '会計月度（1〜12）';
COMMENT ON COLUMN "月次勘定科目残高"."月初残高" IS '月初時点の残高';
COMMENT ON COLUMN "月次勘定科目残高"."借方金額" IS '当月の借方合計金額';
COMMENT ON COLUMN "月次勘定科目残高"."貸方金額" IS '当月の貸方合計金額';
COMMENT ON COLUMN "月次勘定科目残高"."月末残高" IS '月末時点の残高（月初残高 + 借方金額 - 貸方金額）';
