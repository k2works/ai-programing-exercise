-- Prisma Database Comments Generator v1.4.0

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
