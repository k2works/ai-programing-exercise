-- Prisma Database Comments Generator v1.4.0

-- 勘定科目マスタ comments
COMMENT ON TABLE "勘定科目マスタ" IS '勘定科目マスタ（財務会計システムの基本となる勘定科目情報）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目コード" IS '勘定科目コード（主キー）';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目名" IS '勘定科目名';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目カナ" IS '勘定科目カナ';
COMMENT ON COLUMN "勘定科目マスタ"."勘定科目種別" IS '勘定科目種別';
COMMENT ON COLUMN "勘定科目マスタ"."合計科目" IS '合計科目';
COMMENT ON COLUMN "勘定科目マスタ"."BSPL区分" IS 'BSPL区分';
COMMENT ON COLUMN "勘定科目マスタ"."取引要素区分" IS '取引要素区分';
COMMENT ON COLUMN "勘定科目マスタ"."費用区分" IS '費用区分';
COMMENT ON COLUMN "勘定科目マスタ"."表示順序" IS '表示順序';
COMMENT ON COLUMN "勘定科目マスタ"."集計対象" IS '集計対象';
COMMENT ON COLUMN "勘定科目マスタ"."課税取引コード" IS '課税取引コード';
COMMENT ON COLUMN "勘定科目マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "勘定科目マスタ"."更新日時" IS '更新日時';

-- 課税取引マスタ comments
COMMENT ON TABLE "課税取引マスタ" IS '課税取引マスタ（消費税計算のための課税区分情報）';
COMMENT ON COLUMN "課税取引マスタ"."課税取引コード" IS '課税取引コード（主キー）';
COMMENT ON COLUMN "課税取引マスタ"."課税取引名" IS '課税取引名';
COMMENT ON COLUMN "課税取引マスタ"."税率" IS '税率（%）';
COMMENT ON COLUMN "課税取引マスタ"."課税区分" IS '課税区分（課税、非課税、免税、不課税）';
COMMENT ON COLUMN "課税取引マスタ"."説明" IS '説明';
COMMENT ON COLUMN "課税取引マスタ"."適用開始日" IS '適用開始日';
COMMENT ON COLUMN "課税取引マスタ"."適用終了日" IS '適用終了日';
COMMENT ON COLUMN "課税取引マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "課税取引マスタ"."更新日時" IS '更新日時';

-- 勘定科目構成マスタ comments
COMMENT ON TABLE "勘定科目構成マスタ" IS '勘定科目構成マスタ（チルダ連結方式による階層構造管理）';
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目コード" IS '勘定科目コード（主キー、外部キー）';
COMMENT ON COLUMN "勘定科目構成マスタ"."勘定科目パス" IS '勘定科目パス（チルダ連結、例：11^11000^11190^11110）';
COMMENT ON COLUMN "勘定科目構成マスタ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "勘定科目構成マスタ"."更新日時" IS '更新日時';

-- 仕訳エントリ comments
COMMENT ON TABLE "仕訳エントリ" IS '仕訳エントリ（複式簿記の仕訳データ）';
COMMENT ON COLUMN "仕訳エントリ"."伝票番号" IS '伝票番号（主キー）';
COMMENT ON COLUMN "仕訳エントリ"."仕訳日" IS '仕訳日';
COMMENT ON COLUMN "仕訳エントリ"."摘要" IS '摘要';
COMMENT ON COLUMN "仕訳エントリ"."合計金額" IS '合計金額';
COMMENT ON COLUMN "仕訳エントリ"."参照番号" IS '参照番号';
COMMENT ON COLUMN "仕訳エントリ"."作成者" IS '作成者';
COMMENT ON COLUMN "仕訳エントリ"."作成日時" IS '作成日時';
COMMENT ON COLUMN "仕訳エントリ"."更新者" IS '更新者';
COMMENT ON COLUMN "仕訳エントリ"."更新日時" IS '更新日時';

-- 仕訳明細 comments
COMMENT ON TABLE "仕訳明細" IS '仕訳明細（仕訳エントリの明細行データ）';
COMMENT ON COLUMN "仕訳明細"."伝票番号" IS '伝票番号（複合主キー1）';
COMMENT ON COLUMN "仕訳明細"."行番号" IS '行番号（複合主キー2）';
COMMENT ON COLUMN "仕訳明細"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "仕訳明細"."借方金額" IS '借方金額';
COMMENT ON COLUMN "仕訳明細"."貸方金額" IS '貸方金額';
COMMENT ON COLUMN "仕訳明細"."摘要" IS '摘要';
COMMENT ON COLUMN "仕訳明細"."消費税額" IS '消費税額';
COMMENT ON COLUMN "仕訳明細"."消費税率" IS '消費税率';

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

-- 自動仕訳管理 comments
COMMENT ON TABLE "自動仕訳管理" IS '自動仕訳管理（日付管理方式で最終処理日時を管理）';
COMMENT ON COLUMN "自動仕訳管理"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳管理"."ソーステーブル名" IS 'ソーステーブル名';
COMMENT ON COLUMN "自動仕訳管理"."最終処理日時" IS '最終処理日時';
COMMENT ON COLUMN "自動仕訳管理"."作成日時" IS '作成日時';
COMMENT ON COLUMN "自動仕訳管理"."更新日時" IS '更新日時';

-- 自動仕訳パターン comments
COMMENT ON TABLE "自動仕訳パターン" IS '自動仕訳パターン（仕訳生成ルールを管理）';
COMMENT ON COLUMN "自動仕訳パターン"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳パターン"."パターンコード" IS 'パターンコード';
COMMENT ON COLUMN "自動仕訳パターン"."パターン名" IS 'パターン名';
COMMENT ON COLUMN "自動仕訳パターン"."ソーステーブル名" IS 'ソーステーブル名';
COMMENT ON COLUMN "自動仕訳パターン"."説明" IS '説明';
COMMENT ON COLUMN "自動仕訳パターン"."有効フラグ" IS '有効フラグ';
COMMENT ON COLUMN "自動仕訳パターン"."作成日時" IS '作成日時';
COMMENT ON COLUMN "自動仕訳パターン"."更新日時" IS '更新日時';

-- 自動仕訳パターン明細 comments
COMMENT ON TABLE "自動仕訳パターン明細" IS '自動仕訳パターン明細（借方・貸方のマッピング定義）';
COMMENT ON COLUMN "自動仕訳パターン明細"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳パターン明細"."パターンID" IS 'パターンID（外部キー）';
COMMENT ON COLUMN "自動仕訳パターン明細"."行番号" IS '行番号';
COMMENT ON COLUMN "自動仕訳パターン明細"."借方貸方区分" IS '借方・貸方区分（D=借方、C=貸方）';
COMMENT ON COLUMN "自動仕訳パターン明細"."勘定科目コード" IS '勘定科目コード';
COMMENT ON COLUMN "自動仕訳パターン明細"."金額式" IS '金額式（ソースデータのフィールド名や計算式）';
COMMENT ON COLUMN "自動仕訳パターン明細"."摘要テンプレート" IS '摘要テンプレート';
COMMENT ON COLUMN "自動仕訳パターン明細"."作成日時" IS '作成日時';
COMMENT ON COLUMN "自動仕訳パターン明細"."更新日時" IS '更新日時';

-- 自動仕訳ログ comments
COMMENT ON TABLE "自動仕訳ログ" IS '自動仕訳実行ログ（実行履歴とエラー管理）';
COMMENT ON COLUMN "自動仕訳ログ"."ID" IS 'ID（主キー）';
COMMENT ON COLUMN "自動仕訳ログ"."パターンID" IS 'パターンID（外部キー）';
COMMENT ON COLUMN "自動仕訳ログ"."実行日時" IS '実行日時';
COMMENT ON COLUMN "自動仕訳ログ"."処理レコード数" IS '処理レコード数';
COMMENT ON COLUMN "自動仕訳ログ"."生成仕訳数" IS '生成仕訳数';
COMMENT ON COLUMN "自動仕訳ログ"."ステータス" IS 'ステータス（成功、エラー、警告）';
COMMENT ON COLUMN "自動仕訳ログ"."メッセージ" IS 'メッセージ';
COMMENT ON COLUMN "自動仕訳ログ"."エラー詳細" IS 'エラー詳細';
COMMENT ON COLUMN "自動仕訳ログ"."作成日時" IS '作成日時';
