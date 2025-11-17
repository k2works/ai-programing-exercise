-- CreateTable
CREATE TABLE "勘定科目構成マスタ" (
    "勘定科目コード" VARCHAR(20) NOT NULL,
    "勘定科目パス" VARCHAR(200) NOT NULL,
    "作成日時" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "更新日時" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "勘定科目構成マスタ_pkey" PRIMARY KEY ("勘定科目コード")
);

-- CreateIndex
CREATE INDEX "idx_account_path" ON "勘定科目構成マスタ"("勘定科目パス");

-- AddForeignKey
ALTER TABLE "勘定科目構成マスタ" ADD CONSTRAINT "勘定科目構成マスタ_勘定科目コード_fkey" FOREIGN KEY ("勘定科目コード") REFERENCES "勘定科目マスタ"("勘定科目コード") ON DELETE CASCADE ON UPDATE CASCADE;
