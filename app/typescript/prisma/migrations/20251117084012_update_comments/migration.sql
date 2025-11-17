-- Prisma Database Comments Generator v1.4.0

-- 日次勘定科目残高 comments
COMMENT ON TABLE "日次勘定科目残高" IS '日次勘定科目残高（日ごとの借方・貸方金額を記録）';
COMMENT ON COLUMN "日次勘定科目残高"."起票日" IS '起票日（複合主キー1）';
COMMENT ON COLUMN "日次勘定科目残高"."勘定科目コード" IS '勘定科目コード（複合主キー2）';
COMMENT ON COLUMN "日次勘定科目残高"."補助科目コード" IS '補助科目コード（複合主キー3）';
COMMENT ON COLUMN "日次勘定科目残高"."部門コード" IS '部門コード（複合主キー4）';
COMMENT ON COLUMN "日次勘定科目残高"."プロジェクトコード" IS 'プロジェクトコード（複合主キー5）';
COMMENT ON COLUMN "日次勘定科目残高"."決算仕訳フラグ" IS '決算仕訳フラグ（複合主キー6）';
COMMENT ON COLUMN "日次勘定科目残高"."借方金額" IS '借方金額';
COMMENT ON COLUMN "日次勘定科目残高"."貸方金額" IS '貸方金額';
COMMENT ON COLUMN "日次勘定科目残高"."作成日時" IS '作成日時';
COMMENT ON COLUMN "日次勘定科目残高"."更新日時" IS '更新日時';

-- 月次勘定科目残高 comments
COMMENT ON TABLE "月次勘定科目残高" IS '月次勘定科目残高（月ごとの月初残高・借方金額・貸方金額・月末残高を記録）';
COMMENT ON COLUMN "月次勘定科目残高"."会計年月" IS '会計年月（複合主キー1）';
COMMENT ON COLUMN "月次勘定科目残高"."勘定科目コード" IS '勘定科目コード（複合主キー2）';
COMMENT ON COLUMN "月次勘定科目残高"."補助科目コード" IS '補助科目コード（複合主キー3）';
COMMENT ON COLUMN "月次勘定科目残高"."部門コード" IS '部門コード（複合主キー4）';
COMMENT ON COLUMN "月次勘定科目残高"."プロジェクトコード" IS 'プロジェクトコード（複合主キー5）';
COMMENT ON COLUMN "月次勘定科目残高"."決算仕訳フラグ" IS '決算仕訳フラグ（複合主キー6）';
COMMENT ON COLUMN "月次勘定科目残高"."月初残高" IS '月初残高';
COMMENT ON COLUMN "月次勘定科目残高"."借方金額" IS '借方金額';
COMMENT ON COLUMN "月次勘定科目残高"."貸方金額" IS '貸方金額';
COMMENT ON COLUMN "月次勘定科目残高"."月末残高" IS '月末残高';
COMMENT ON COLUMN "月次勘定科目残高"."作成日時" IS '作成日時';
COMMENT ON COLUMN "月次勘定科目残高"."更新日時" IS '更新日時';
