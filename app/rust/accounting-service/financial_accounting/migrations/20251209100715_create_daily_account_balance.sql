-- 日次勘定科目残高テーブル
CREATE TABLE "日次勘定科目残高" (
    "起票日" DATE NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "補助科目コード" VARCHAR(10) NOT NULL DEFAULT '',
    "部門コード" VARCHAR(5) NOT NULL DEFAULT '',
    "プロジェクトコード" VARCHAR(10) NOT NULL DEFAULT '',
    "決算仕訳フラグ" INTEGER NOT NULL DEFAULT 0,
    "借方金額" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "貸方金額" DECIMAL(15, 2) NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
);

-- 外部キー制約
ALTER TABLE "日次勘定科目残高"
    ADD CONSTRAINT "fk_日次勘定科目残高_勘定科目マスタ"
    FOREIGN KEY ("勘定科目コード")
    REFERENCES "勘定科目マスタ"("勘定科目コード");

-- インデックス作成
CREATE INDEX "idx_日次勘定科目残高_起票日" ON "日次勘定科目残高"("起票日");
CREATE INDEX "idx_日次勘定科目残高_勘定科目" ON "日次勘定科目残高"("勘定科目コード");
CREATE INDEX "idx_日次勘定科目残高_部門" ON "日次勘定科目残高"("部門コード");
CREATE INDEX "idx_日次勘定科目残高_プロジェクト" ON "日次勘定科目残高"("プロジェクトコード");

-- CHECK 制約
ALTER TABLE "日次勘定科目残高"
    ADD CONSTRAINT "check_日次残高_借方金額"
    CHECK ("借方金額" >= 0);

ALTER TABLE "日次勘定科目残高"
    ADD CONSTRAINT "check_日次残高_貸方金額"
    CHECK ("貸方金額" >= 0);

ALTER TABLE "日次勘定科目残高"
    ADD CONSTRAINT "check_日次残高_決算仕訳フラグ"
    CHECK ("決算仕訳フラグ" IN (0, 1));

-- コメント追加
COMMENT ON TABLE "日次勘定科目残高" IS '日次勘定科目残高（日ごとの借方・貸方金額を記録）';
COMMENT ON COLUMN "日次勘定科目残高"."起票日" IS '実際の取引発生日';
COMMENT ON COLUMN "日次勘定科目残高"."勘定科目コード" IS '勘定科目マスタの外部キー';
COMMENT ON COLUMN "日次勘定科目残高"."補助科目コード" IS '補助科目（得意先、仕入先など）';
COMMENT ON COLUMN "日次勘定科目残高"."部門コード" IS '部門別管理用';
COMMENT ON COLUMN "日次勘定科目残高"."プロジェクトコード" IS 'プロジェクト別管理用';
COMMENT ON COLUMN "日次勘定科目残高"."決算仕訳フラグ" IS '0=通常仕訳、1=決算仕訳';
COMMENT ON COLUMN "日次勘定科目残高"."借方金額" IS '借方合計金額';
COMMENT ON COLUMN "日次勘定科目残高"."貸方金額" IS '貸方合計金額';
