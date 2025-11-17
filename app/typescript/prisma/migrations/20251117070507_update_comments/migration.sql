-- Prisma Database Comments Generator v1.4.0

-- 仕訳 comments
COMMENT ON TABLE "仕訳" IS '仕訳（ヘッダー：3層構造）';
COMMENT ON COLUMN "仕訳"."仕訳伝票番号" IS '仕訳伝票番号（主キー）';
COMMENT ON COLUMN "仕訳"."起票日" IS '起票日';
COMMENT ON COLUMN "仕訳"."入力日" IS '入力日';
COMMENT ON COLUMN "仕訳"."決算仕訳フラグ" IS '決算仕訳フラグ';
COMMENT ON COLUMN "仕訳"."単振フラグ" IS '単振フラグ';
COMMENT ON COLUMN "仕訳"."仕訳伝票区分" IS '仕訳伝票区分';
COMMENT ON COLUMN "仕訳"."定期計上フラグ" IS '定期計上フラグ';
COMMENT ON COLUMN "仕訳"."社員コード" IS '社員コード';
COMMENT ON COLUMN "仕訳"."部門コード" IS '部門コード';
COMMENT ON COLUMN "仕訳"."赤伝フラグ" IS '赤伝フラグ';
COMMENT ON COLUMN "仕訳"."赤黒伝票番号" IS '赤黒伝票番号';
COMMENT ON COLUMN "仕訳"."作成日時" IS '作成日時';
COMMENT ON COLUMN "仕訳"."更新日時" IS '更新日時';

-- 仕訳明細_3層 comments
COMMENT ON TABLE "仕訳明細_3層" IS '仕訳明細（行摘要：3層構造）';
COMMENT ON COLUMN "仕訳明細_3層"."仕訳伝票番号" IS '仕訳伝票番号（複合主キー1）';
COMMENT ON COLUMN "仕訳明細_3層"."仕訳行番号" IS '仕訳行番号（複合主キー2）';
COMMENT ON COLUMN "仕訳明細_3層"."行摘要" IS '行摘要';
COMMENT ON COLUMN "仕訳明細_3層"."作成日時" IS '作成日時';
COMMENT ON COLUMN "仕訳明細_3層"."更新日時" IS '更新日時';

-- 仕訳貸借明細 comments
COMMENT ON TABLE "仕訳貸借明細" IS '仕訳貸借明細（借方・貸方の詳細：3層構造）';
COMMENT ON COLUMN "仕訳貸借明細"."仕訳伝票番号" IS '仕訳伝票番号（複合主キー1）';
COMMENT ON COLUMN "仕訳貸借明細"."仕訳行番号" IS '仕訳行番号（複合主キー2）';
COMMENT ON COLUMN "仕訳貸借明細"."仕訳行貸借区分" IS '仕訳行貸借区分（複合主キー3）';
COMMENT ON COLUMN "仕訳貸借明細"."通貨コード" IS '通貨コード';
COMMENT ON COLUMN "仕訳貸借明細"."為替レート" IS '為替レート';
COMMENT ON COLUMN "仕訳貸借明細"."部門コード" IS '部門コード';
COMMENT ON COLUMN "仕訳貸借明細"."プロジェクトコード" IS 'プロジェクトコード';
COMMENT ON COLUMN "仕訳貸借明細"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "仕訳貸借明細"."補助科目コード" IS '補助科目コード';
COMMENT ON COLUMN "仕訳貸借明細"."仕訳金額" IS '仕訳金額';
COMMENT ON COLUMN "仕訳貸借明細"."基軸換算仕訳金額" IS '基軸換算仕訳金額';
COMMENT ON COLUMN "仕訳貸借明細"."消費税区分" IS '消費税区分';
COMMENT ON COLUMN "仕訳貸借明細"."消費税率" IS '消費税率';
COMMENT ON COLUMN "仕訳貸借明細"."消費税計算区分" IS '消費税計算区分';
COMMENT ON COLUMN "仕訳貸借明細"."期日" IS '期日';
COMMENT ON COLUMN "仕訳貸借明細"."資金繰フラグ" IS '資金繰フラグ';
COMMENT ON COLUMN "仕訳貸借明細"."セグメントコード" IS 'セグメントコード';
COMMENT ON COLUMN "仕訳貸借明細"."相手勘定科目コード" IS '相手勘定科目コード';
COMMENT ON COLUMN "仕訳貸借明細"."相手補助科目コード" IS '相手補助科目コード';
COMMENT ON COLUMN "仕訳貸借明細"."付箋コード" IS '付箋コード';
COMMENT ON COLUMN "仕訳貸借明細"."付箋内容" IS '付箋内容';
COMMENT ON COLUMN "仕訳貸借明細"."作成日時" IS '作成日時';
COMMENT ON COLUMN "仕訳貸借明細"."更新日時" IS '更新日時';
