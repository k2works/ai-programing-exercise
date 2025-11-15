-- 発注テーブルの作成
CREATE TABLE IF NOT EXISTS 発注 (
  発注番号 VARCHAR(13) PRIMARY KEY,
  発注日 TIMESTAMP NOT NULL,
  部門コード VARCHAR(6) NOT NULL,
  開始日 TIMESTAMP NOT NULL,
  仕入先コード VARCHAR(8) NOT NULL,
  仕入先枝番 INTEGER NOT NULL DEFAULT 0,
  社員コード VARCHAR(10) NOT NULL,
  完了フラグ INTEGER NOT NULL DEFAULT 0,
  作成日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  作成者名 VARCHAR(12),
  更新日時 TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  更新者名 VARCHAR(12),
  CONSTRAINT fk_po_department FOREIGN KEY (部門コード, 開始日)
    REFERENCES 部門マスタ(部門コード, 開始日),
  CONSTRAINT fk_po_supplier FOREIGN KEY (仕入先コード, 仕入先枝番)
    REFERENCES 仕入先マスタ(仕入先コード, 仕入先枝番),
  CONSTRAINT fk_po_employee FOREIGN KEY (社員コード)
    REFERENCES 社員マスタ(社員コード)
);

-- インデックスの作成
CREATE INDEX idx_po_supplier ON 発注(仕入先コード, 仕入先枝番);
CREATE INDEX idx_po_date ON 発注(発注日);
CREATE INDEX idx_po_employee ON 発注(社員コード);

-- コメントの追加
COMMENT ON TABLE 発注 IS '仕入先への発注情報を管理するヘッダーテーブル';
COMMENT ON COLUMN 発注.発注番号 IS '発注を一意に識別する番号';
COMMENT ON COLUMN 発注.発注日 IS '発注を行った日時';
COMMENT ON COLUMN 発注.部門コード IS '発注を行った部門のコード';
COMMENT ON COLUMN 発注.開始日 IS '部門マスタの有効期間開始日';
COMMENT ON COLUMN 発注.仕入先コード IS '発注先の仕入先コード';
COMMENT ON COLUMN 発注.仕入先枝番 IS '仕入先の枝番';
COMMENT ON COLUMN 発注.社員コード IS '発注を行った社員のコード';
COMMENT ON COLUMN 発注.完了フラグ IS '発注が完了したかどうか（0:未完了、1:完了）';
