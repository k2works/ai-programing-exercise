-- CreateTable
CREATE TABLE "accounts" (
    "accountCode" VARCHAR(10) NOT NULL,
    "accountName" VARCHAR(40) NOT NULL,
    "accountType" VARCHAR(10) NOT NULL,
    "sumAccount" BOOLEAN NOT NULL DEFAULT false,
    "bsplDistinction" CHAR(1),
    "displayOrder" INTEGER,
    "aggregationTarget" BOOLEAN NOT NULL DEFAULT true,
    "taxCode" VARCHAR(2),
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "accounts_pkey" PRIMARY KEY ("accountCode")
);

-- CreateTable
CREATE TABLE "tax_transactions" (
    "taxCode" VARCHAR(2) NOT NULL,
    "taxName" VARCHAR(40) NOT NULL,
    "taxRate" REAL NOT NULL,
    "taxType" VARCHAR(10) NOT NULL,
    "description" VARCHAR(200),
    "validFrom" TIMESTAMP(3),
    "validTo" TIMESTAMP(3),
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "tax_transactions_pkey" PRIMARY KEY ("taxCode")
);

-- CreateTable
CREATE TABLE "account_structures" (
    "accountCode" VARCHAR(10) NOT NULL,
    "parentPath" VARCHAR(1000),
    "hierarchyLevel" INTEGER NOT NULL,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "account_structures_pkey" PRIMARY KEY ("accountCode")
);

-- CreateTable
CREATE TABLE "accounting_periods" (
    "fiscalYear" INTEGER NOT NULL,
    "periodName" VARCHAR(20) NOT NULL,
    "startDate" TIMESTAMP(3) NOT NULL,
    "endDate" TIMESTAMP(3) NOT NULL,
    "isClosed" BOOLEAN NOT NULL DEFAULT false,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "accounting_periods_pkey" PRIMARY KEY ("fiscalYear")
);

-- CreateTable
CREATE TABLE "journals" (
    "id" SERIAL NOT NULL,
    "journalDate" TIMESTAMP(3) NOT NULL,
    "fiscalYear" INTEGER NOT NULL,
    "description" VARCHAR(200) NOT NULL,
    "userId" VARCHAR(50),
    "userName" VARCHAR(50),
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "journals_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "journal_detail_items" (
    "id" SERIAL NOT NULL,
    "journalId" INTEGER NOT NULL,
    "accountCode" VARCHAR(10) NOT NULL,
    "debitAmount" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "creditAmount" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "description" VARCHAR(200),
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "journal_detail_items_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "daily_account_balances" (
    "id" SERIAL NOT NULL,
    "accountCode" VARCHAR(10) NOT NULL,
    "balanceDate" TIMESTAMP(3) NOT NULL,
    "openingBalance" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "debitAmount" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "creditAmount" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "closingBalance" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "daily_account_balances_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "monthly_account_balances" (
    "id" SERIAL NOT NULL,
    "fiscalYearMonth" CHAR(6) NOT NULL,
    "accountCode" VARCHAR(10) NOT NULL,
    "openingBalance" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "debitAmount" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "creditAmount" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "closingBalance" DECIMAL(15,2) NOT NULL DEFAULT 0,
    "createdAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "monthly_account_balances_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "audit_logs" (
    "id" SERIAL NOT NULL,
    "entityType" VARCHAR(50) NOT NULL,
    "entityId" VARCHAR(50) NOT NULL,
    "action" VARCHAR(20) NOT NULL,
    "userId" VARCHAR(50),
    "userName" VARCHAR(50),
    "timestamp" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "oldValues" JSONB,
    "newValues" JSONB,
    "changes" JSONB,
    "reason" VARCHAR(200),
    "ipAddress" VARCHAR(45),
    "userAgent" VARCHAR(500),

    CONSTRAINT "audit_logs_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE INDEX "accounts_bsplDistinction_idx" ON "accounts"("bsplDistinction");

-- CreateIndex
CREATE INDEX "accounts_accountType_idx" ON "accounts"("accountType");

-- CreateIndex
CREATE INDEX "accounts_displayOrder_idx" ON "accounts"("displayOrder");

-- CreateIndex
CREATE INDEX "accounts_taxCode_idx" ON "accounts"("taxCode");

-- CreateIndex
CREATE INDEX "tax_transactions_taxType_idx" ON "tax_transactions"("taxType");

-- CreateIndex
CREATE INDEX "tax_transactions_validFrom_validTo_idx" ON "tax_transactions"("validFrom", "validTo");

-- CreateIndex
CREATE INDEX "account_structures_parentPath_idx" ON "account_structures"("parentPath");

-- CreateIndex
CREATE INDEX "account_structures_hierarchyLevel_idx" ON "account_structures"("hierarchyLevel");

-- CreateIndex
CREATE INDEX "accounting_periods_startDate_endDate_idx" ON "accounting_periods"("startDate", "endDate");

-- CreateIndex
CREATE INDEX "journals_journalDate_idx" ON "journals"("journalDate");

-- CreateIndex
CREATE INDEX "journals_fiscalYear_idx" ON "journals"("fiscalYear");

-- CreateIndex
CREATE INDEX "journal_detail_items_journalId_idx" ON "journal_detail_items"("journalId");

-- CreateIndex
CREATE INDEX "journal_detail_items_accountCode_idx" ON "journal_detail_items"("accountCode");

-- CreateIndex
CREATE INDEX "daily_account_balances_balanceDate_idx" ON "daily_account_balances"("balanceDate");

-- CreateIndex
CREATE UNIQUE INDEX "daily_account_balances_accountCode_balanceDate_key" ON "daily_account_balances"("accountCode", "balanceDate");

-- CreateIndex
CREATE INDEX "monthly_account_balances_fiscalYearMonth_idx" ON "monthly_account_balances"("fiscalYearMonth");

-- CreateIndex
CREATE UNIQUE INDEX "monthly_account_balances_fiscalYearMonth_accountCode_key" ON "monthly_account_balances"("fiscalYearMonth", "accountCode");

-- CreateIndex
CREATE INDEX "audit_logs_entityType_entityId_idx" ON "audit_logs"("entityType", "entityId");

-- CreateIndex
CREATE INDEX "audit_logs_timestamp_idx" ON "audit_logs"("timestamp");

-- CreateIndex
CREATE INDEX "audit_logs_userId_idx" ON "audit_logs"("userId");

-- AddForeignKey
ALTER TABLE "accounts" ADD CONSTRAINT "accounts_taxCode_fkey" FOREIGN KEY ("taxCode") REFERENCES "tax_transactions"("taxCode") ON DELETE SET NULL ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "account_structures" ADD CONSTRAINT "account_structures_accountCode_fkey" FOREIGN KEY ("accountCode") REFERENCES "accounts"("accountCode") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "journals" ADD CONSTRAINT "journals_fiscalYear_fkey" FOREIGN KEY ("fiscalYear") REFERENCES "accounting_periods"("fiscalYear") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "journal_detail_items" ADD CONSTRAINT "journal_detail_items_journalId_fkey" FOREIGN KEY ("journalId") REFERENCES "journals"("id") ON DELETE CASCADE ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "journal_detail_items" ADD CONSTRAINT "journal_detail_items_accountCode_fkey" FOREIGN KEY ("accountCode") REFERENCES "accounts"("accountCode") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "daily_account_balances" ADD CONSTRAINT "daily_account_balances_accountCode_fkey" FOREIGN KEY ("accountCode") REFERENCES "accounts"("accountCode") ON DELETE RESTRICT ON UPDATE CASCADE;

-- AddForeignKey
ALTER TABLE "monthly_account_balances" ADD CONSTRAINT "monthly_account_balances_accountCode_fkey" FOREIGN KEY ("accountCode") REFERENCES "accounts"("accountCode") ON DELETE RESTRICT ON UPDATE CASCADE;
