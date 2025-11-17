-- CreateTable
CREATE TABLE "勘定科目マスタ" (
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "勘定科目名" VARCHAR(40) NOT NULL,
    "勘定科目カナ" VARCHAR(40),
    "勘定科目種別" VARCHAR(10) NOT NULL,
    "合計科目" BOOLEAN NOT NULL DEFAULT false,
    "BSPL区分" CHAR(1),
    "取引要素区分" CHAR(1),
    "費用区分" CHAR(1),
    "表示順序" INTEGER,
    "集計対象" BOOLEAN NOT NULL DEFAULT true,
    "課税取引コード" VARCHAR(2),
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "勘定科目マスタ_pkey" PRIMARY KEY ("勘定科目コード")
);

-- CreateTable
CREATE TABLE "課税取引マスタ" (
    "課税取引コード" VARCHAR(2) NOT NULL,
    "課税取引名" VARCHAR(40) NOT NULL,
    "税率" REAL NOT NULL,
    "課税区分" VARCHAR(10) NOT NULL,
    "説明" VARCHAR(200),
    "適用開始日" TIMESTAMP(3),
    "適用終了日" TIMESTAMP(3),
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "課税取引マスタ_pkey" PRIMARY KEY ("課税取引コード")
);

-- CreateTable
CREATE TABLE "勘定科目構成マスタ" (
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "勘定科目パス" VARCHAR(200) NOT NULL,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "勘定科目構成マスタ_pkey" PRIMARY KEY ("勘定科目コード")
);

-- CreateTable
CREATE TABLE "仕訳エントリ" (
    "伝票番号" VARCHAR(10) NOT NULL,
    "仕訳日" DATE NOT NULL,
    "摘要" VARCHAR(100) NOT NULL,
    "合計金額" DECIMAL(15,2) NOT NULL,
    "参照番号" VARCHAR(20),
    "作成者" VARCHAR(20) NOT NULL,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新者" VARCHAR(20),
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "仕訳エントリ_pkey" PRIMARY KEY ("伝票番号")
);

-- CreateTable
CREATE TABLE "仕訳明細" (
    "伝票番号" VARCHAR(10) NOT NULL,
    "行番号" INTEGER NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "借方金額" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "貸方金額" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "摘要" VARCHAR(100) NOT NULL,
    "消費税額" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "消費税率" DECIMAL(5,2),

    CONSTRAINT "仕訳明細_pkey" PRIMARY KEY ("伝票番号","行番号")
);

-- CreateTable
CREATE TABLE "仕訳" (
    "仕訳伝票番号" VARCHAR(10) NOT NULL,
    "起票日" DATE NOT NULL,
    "入力日" DATE NOT NULL,
    "決算仕訳フラグ" SMALLINT NOT NULL DEFAULT 0,
    "単振フラグ" SMALLINT NOT NULL DEFAULT 1,
    "仕訳伝票区分" SMALLINT NOT NULL,
    "定期計上フラグ" SMALLINT NOT NULL DEFAULT 0,
    "社員コード" VARCHAR(10),
    "部門コード" VARCHAR(5),
    "赤伝フラグ" SMALLINT NOT NULL DEFAULT 0,
    "赤黒伝票番号" INTEGER,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "仕訳_pkey" PRIMARY KEY ("仕訳伝票番号")
);

-- CreateTable
CREATE TABLE "仕訳明細_3層" (
    "仕訳伝票番号" VARCHAR(10) NOT NULL,
    "仕訳行番号" SMALLINT NOT NULL,
    "行摘要" VARCHAR(1000) NOT NULL,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "仕訳明細_3層_pkey" PRIMARY KEY ("仕訳伝票番号","仕訳行番号")
);

-- CreateTable
CREATE TABLE "仕訳貸借明細" (
    "仕訳伝票番号" VARCHAR(10) NOT NULL,
    "仕訳行番号" SMALLINT NOT NULL,
    "仕訳行貸借区分" VARCHAR(1) NOT NULL,
    "通貨コード" VARCHAR(3) NOT NULL,
    "為替レート" DECIMAL(8,2) NOT NULL,
    "部門コード" VARCHAR(3),
    "プロジェクトコード" VARCHAR(10),
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "補助科目コード" VARCHAR(10),
    "仕訳金額" DECIMAL(14,2) NOT NULL,
    "基軸換算仕訳金額" DECIMAL(14,2) NOT NULL,
    "消費税区分" VARCHAR(2),
    "消費税率" SMALLINT,
    "消費税計算区分" VARCHAR(2),
    "期日" DATE,
    "資金繰フラグ" SMALLINT NOT NULL DEFAULT 0,
    "セグメントコード" VARCHAR(10),
    "相手勘定科目コード" VARCHAR(10),
    "相手補助科目コード" VARCHAR(10),
    "付箋コード" VARCHAR(1),
    "付箋内容" VARCHAR(60),
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "仕訳貸借明細_pkey" PRIMARY KEY ("仕訳伝票番号","仕訳行番号","仕訳行貸借区分")
);

-- CreateTable
CREATE TABLE "自動仕訳管理" (
    "ID" SERIAL NOT NULL,
    "ソーステーブル名" VARCHAR(100) NOT NULL,
    "最終処理日時" TIMESTAMP(3) NOT NULL,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "自動仕訳管理_pkey" PRIMARY KEY ("ID")
);

-- CreateTable
CREATE TABLE "自動仕訳パターン" (
    "ID" SERIAL NOT NULL,
    "パターンコード" VARCHAR(20) NOT NULL,
    "パターン名" VARCHAR(100) NOT NULL,
    "ソーステーブル名" VARCHAR(100) NOT NULL,
    "説明" VARCHAR(500),
    "有効フラグ" BOOLEAN NOT NULL DEFAULT true,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "自動仕訳パターン_pkey" PRIMARY KEY ("ID")
);

-- CreateTable
CREATE TABLE "自動仕訳パターン明細" (
    "ID" SERIAL NOT NULL,
    "パターンID" INTEGER NOT NULL,
    "行番号" INTEGER NOT NULL,
    "借方貸方区分" VARCHAR(1) NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "金額式" VARCHAR(200) NOT NULL,
    "摘要テンプレート" VARCHAR(200),
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "自動仕訳パターン明細_pkey" PRIMARY KEY ("ID")
);

-- CreateTable
CREATE TABLE "自動仕訳ログ" (
    "ID" SERIAL NOT NULL,
    "パターンID" INTEGER NOT NULL,
    "実行日時" TIMESTAMP(3) NOT NULL,
    "処理レコード数" INTEGER NOT NULL,
    "生成仕訳数" INTEGER NOT NULL,
    "ステータス" VARCHAR(20) NOT NULL,
    "メッセージ" VARCHAR(500),
    "エラー詳細" TEXT,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "自動仕訳ログ_pkey" PRIMARY KEY ("ID")
);

-- CreateIndex
CREATE INDEX "idx_bspl_distinction" ON "勘定科目マスタ"("BSPL区分");

-- CreateIndex
CREATE INDEX "idx_transaction_distinction" ON "勘定科目マスタ"("取引要素区分");

-- CreateIndex
CREATE INDEX "idx_account_type" ON "勘定科目マスタ"("勘定科目種別");

-- CreateIndex
CREATE INDEX "idx_display_order" ON "勘定科目マスタ"("表示順序");

-- CreateIndex
CREATE INDEX "idx_tax_code" ON "勘定科目マスタ"("課税取引コード");

-- CreateIndex
CREATE INDEX "idx_tax_type" ON "課税取引マスタ"("課税区分");

-- CreateIndex
CREATE INDEX "idx_valid_period" ON "課税取引マスタ"("適用開始日", "適用終了日");

-- CreateIndex
CREATE INDEX "idx_account_path" ON "勘定科目構成マスタ"("勘定科目パス");

-- CreateIndex
CREATE INDEX "idx_journal_date" ON "仕訳"("起票日");

-- CreateIndex
CREATE INDEX "idx_journal_department" ON "仕訳"("部門コード");

-- CreateIndex
CREATE INDEX "idx_red_slip" ON "仕訳"("赤伝フラグ");

-- CreateIndex
CREATE INDEX "idx_detail_item_account" ON "仕訳貸借明細"("勘定科目コード");

-- CreateIndex
CREATE INDEX "idx_detail_item_department" ON "仕訳貸借明細"("部門コード");

-- CreateIndex
CREATE INDEX "idx_detail_item_project" ON "仕訳貸借明細"("プロジェクトコード");

-- CreateIndex
CREATE UNIQUE INDEX "自動仕訳管理_ソーステーブル名_key" ON "自動仕訳管理"("ソーステーブル名");

-- CreateIndex
CREATE INDEX "idx_auto_journal_source" ON "自動仕訳管理"("ソーステーブル名");

-- CreateIndex
CREATE UNIQUE INDEX "自動仕訳パターン_パターンコード_key" ON "自動仕訳パターン"("パターンコード");

-- CreateIndex
CREATE INDEX "idx_auto_pattern_source" ON "自動仕訳パターン"("ソーステーブル名");

-- CreateIndex
CREATE INDEX "idx_auto_pattern_active" ON "自動仕訳パターン"("有効フラグ");

-- CreateIndex
CREATE INDEX "idx_auto_pattern_item" ON "自動仕訳パターン明細"("パターンID");

-- CreateIndex
CREATE INDEX "idx_auto_log_pattern" ON "自動仕訳ログ"("パターンID");

-- CreateIndex
CREATE INDEX "idx_auto_log_executed" ON "自動仕訳ログ"("実行日時");

-- CreateIndex
CREATE INDEX "idx_auto_log_status" ON "自動仕訳ログ"("ステータス");

-- AddForeignKey
ALTER TABLE "勘定科目マスタ" ADD CONSTRAINT "勘定科目マスタ_課税取引コード_fkey" FOREIGN KEY ("課税取引コード") REFERENCES "課税取引マスタ"("課税取引コード") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "勘定科目構成マスタ" ADD CONSTRAINT "勘定科目構成マスタ_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "仕訳明細" ADD CONSTRAINT "仕訳明細_伝票番号_fkey" FOREIGN KEY ("伝票番号") REFERENCES "仕訳エントリ"("伝票番号") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "仕訳明細" ADD CONSTRAINT "仕訳明細_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "仕訳明細_3層" ADD CONSTRAINT "仕訳明細_3層_仕訳伝票番号_fkey" FOREIGN KEY ("仕訳伝票番号") REFERENCES "仕訳"("仕訳伝票番号") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "仕訳貸借明細" ADD CONSTRAINT "仕訳貸借明細_仕訳伝票番号_仕訳行番号_fkey" FOREIGN KEY ("仕訳伝票番号", "仕訳行番号") REFERENCES "仕訳明細_3層"("仕訳伝票番号", "仕訳行番号") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "仕訳貸借明細" ADD CONSTRAINT "仕訳貸借明細_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "自動仕訳パターン明細" ADD CONSTRAINT "自動仕訳パターン明細_パターンID_fkey" FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン"("ID") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "自動仕訳ログ" ADD CONSTRAINT "自動仕訳ログ_パターンID_fkey" FOREIGN KEY ("パターンID") REFERENCES "自動仕訳パターン"("ID") ON DELETE CASCADE ON UPDATE CASCADE;
