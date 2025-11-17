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

-- AddForeignKey
ALTER TABLE "勘定科目マスタ" ADD CONSTRAINT "勘定科目マスタ_課税取引コード_fkey" FOREIGN KEY ("課税取引コード") REFERENCES "課税取引マスタ"("課税取引コード") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "勘定科目構成マスタ" ADD CONSTRAINT "勘定科目構成マスタ_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE CASCADE ON UPDATE CASCADE;
