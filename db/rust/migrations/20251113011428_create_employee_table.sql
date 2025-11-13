-- 社員マスタテーブルの作成
CREATE TABLE "社員マスタ" (
    "社員コード" VARCHAR(10) NOT NULL,
    "社員名" VARCHAR(20),
    "社員名カナ" VARCHAR(40),
    "パスワード" VARCHAR(8),
    "電話番号" VARCHAR(13),
    "FAX番号" VARCHAR(13),
    "部門コード" VARCHAR(6) NOT NULL,
    "開始日" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_DATE,
    "職種コード" VARCHAR(2) NOT NULL,
    "承認権限コード" VARCHAR(2) NOT NULL,
    "作成日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_DATE,
    "作成者名" VARCHAR(12),
    "更新日時" TIMESTAMP(6) NOT NULL DEFAULT CURRENT_DATE,
    "更新者名" VARCHAR(12),

    CONSTRAINT pk_employee PRIMARY KEY ("社員コード")

    -- 部門マスタへの外部キー制約
    -- 注：部門マスタは複合主キー（部門コード、開始日）のため、
    -- 部門コードのみへの外部キー制約は設定できません。
    -- 参照整合性はアプリケーションレベルで管理します。
    -- CONSTRAINT fk_employee_department
    --     FOREIGN KEY ("部門コード")
    --     REFERENCES "部門マスタ" ("部門コード")
    --     ON DELETE RESTRICT
    --     ON UPDATE CASCADE
);

-- 外部キー用のインデックスを作成（参照性能向上）
CREATE INDEX idx_employee_dept_code ON "社員マスタ" ("部門コード");

-- テーブルにコメントを追加
COMMENT ON TABLE "社員マスタ" IS '社員情報を管理するマスタテーブル';
COMMENT ON COLUMN "社員マスタ"."社員コード" IS '社員を一意に識別するコード';
COMMENT ON COLUMN "社員マスタ"."社員名" IS '社員の氏名';
COMMENT ON COLUMN "社員マスタ"."社員名カナ" IS '社員の氏名（カナ）';
COMMENT ON COLUMN "社員マスタ"."パスワード" IS 'ログイン用パスワード';
COMMENT ON COLUMN "社員マスタ"."電話番号" IS '連絡先電話番号';
COMMENT ON COLUMN "社員マスタ"."FAX番号" IS 'FAX番号';
COMMENT ON COLUMN "社員マスタ"."部門コード" IS '所属部門のコード（外部キー）';
COMMENT ON COLUMN "社員マスタ"."開始日" IS '所属開始日';
COMMENT ON COLUMN "社員マスタ"."職種コード" IS '職種の分類コード';
COMMENT ON COLUMN "社員マスタ"."承認権限コード" IS '承認権限のレベルコード';
