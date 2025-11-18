-- CreateTable
CREATE TABLE "journal_cache" (
    "id" TEXT NOT NULL,
    "journalId" TEXT NOT NULL,
    "fiscalYear" INTEGER NOT NULL,
    "journalDate" TIMESTAMP(3) NOT NULL,
    "totalDebitAmount" DECIMAL(15,2) NOT NULL,
    "totalCreditAmount" DECIMAL(15,2) NOT NULL,
    "receivedAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,

    CONSTRAINT "journal_cache_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "journal_cache_journalId_key" ON "journal_cache"("journalId");

-- CreateIndex
CREATE INDEX "journal_cache_fiscalYear_idx" ON "journal_cache"("fiscalYear");

-- CreateIndex
CREATE INDEX "journal_cache_receivedAt_idx" ON "journal_cache"("receivedAt");
