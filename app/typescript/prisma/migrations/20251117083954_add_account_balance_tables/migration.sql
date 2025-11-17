-- CreateTable
CREATE TABLE "日次勘定科目残高" (
    "起票日" DATE NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "補助科目コード" VARCHAR(10) NOT NULL DEFAULT '',
    "部門コード" VARCHAR(5) NOT NULL DEFAULT '',
    "プロジェクトコード" VARCHAR(10) NOT NULL DEFAULT '',
    "決算仕訳フラグ" SMALLINT NOT NULL DEFAULT 0,
    "借方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
    "貸方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "日次勘定科目残高_pkey" PRIMARY KEY ("起票日","勘定科目コード","補助科目コード","部門コード","プロジェクトコード","決算仕訳フラグ")
);

-- CreateTable
CREATE TABLE "月次勘定科目残高" (
    "会計年月" VARCHAR(6) NOT NULL,
    "勘定科目コード" VARCHAR(10) NOT NULL,
    "補助科目コード" VARCHAR(10) NOT NULL DEFAULT '',
    "部門コード" VARCHAR(5) NOT NULL DEFAULT '',
    "プロジェクトコード" VARCHAR(10) NOT NULL DEFAULT '',
    "決算仕訳フラグ" SMALLINT NOT NULL DEFAULT 0,
    "月初残高" DECIMAL(14,2) NOT NULL DEFAULT 0,
    "借方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
    "貸方金額" DECIMAL(14,2) NOT NULL DEFAULT 0,
    "月末残高" DECIMAL(14,2) NOT NULL DEFAULT 0,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "月次勘定科目残高_pkey" PRIMARY KEY ("会計年月","勘定科目コード","補助科目コード","部門コード","プロジェクトコード","決算仕訳フラグ")
);

-- CreateIndex
CREATE INDEX "idx_daily_balance_account" ON "日次勘定科目残高"("勘定科目コード");

-- CreateIndex
CREATE INDEX "idx_daily_balance_date" ON "日次勘定科目残高"("起票日");

-- CreateIndex
CREATE INDEX "idx_daily_balance_department" ON "日次勘定科目残高"("部門コード");

-- CreateIndex
CREATE INDEX "idx_daily_balance_project" ON "日次勘定科目残高"("プロジェクトコード");

-- CreateIndex
CREATE INDEX "idx_monthly_balance_account" ON "月次勘定科目残高"("勘定科目コード");

-- CreateIndex
CREATE INDEX "idx_monthly_balance_month" ON "月次勘定科目残高"("会計年月");

-- CreateIndex
CREATE INDEX "idx_monthly_balance_department" ON "月次勘定科目残高"("部門コード");

-- CreateIndex
CREATE INDEX "idx_monthly_balance_project" ON "月次勘定科目残高"("プロジェクトコード");

-- AddForeignKey
ALTER TABLE "日次勘定科目残高" ADD CONSTRAINT "日次勘定科目残高_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "月次勘定科目残高" ADD CONSTRAINT "月次勘定科目残高_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE RESTRICT ON UPDATE CASCADE;
