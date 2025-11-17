-- CreateEnum
CREATE TYPE "account_type" AS ENUM ('資産', '負債', '純資産', '収益', '費用');

-- CreateTable
CREATE TABLE "勘定科目マスタ" (
    "勘定科目ID" SERIAL NOT NULL,
    "勘定科目コード" VARCHAR(20) NOT NULL,
    "勘定科目名" VARCHAR(100) NOT NULL,
    "勘定科目種別" "account_type" NOT NULL,
    "残高" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "課税取引コード" VARCHAR(2),
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "勘定科目マスタ_pkey" PRIMARY KEY ("勘定科目ID")
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

-- CreateIndex
CREATE UNIQUE INDEX "勘定科目マスタ_勘定科目コード_key" ON "勘定科目マスタ"("勘定科目コード");

-- CreateIndex
CREATE INDEX "idx_tax_code" ON "勘定科目マスタ"("課税取引コード");

-- CreateIndex
CREATE INDEX "idx_tax_type" ON "課税取引マスタ"("課税区分");

-- CreateIndex
CREATE INDEX "idx_valid_period" ON "課税取引マスタ"("適用開始日", "適用終了日");

-- AddForeignKey
ALTER TABLE "勘定科目マスタ" ADD CONSTRAINT "勘定科目マスタ_課税取引コード_fkey" FOREIGN KEY ("課税取引コード") REFERENCES "課税取引マスタ"("課税取引コード") ON DELETE SET NULL ON UPDATE CASCADE;
