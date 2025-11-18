-- CreateTable
CREATE TABLE "financial_analysis_cache" (
    "id" TEXT NOT NULL,
    "fiscalYear" INTEGER NOT NULL,
    "sales" DECIMAL(15,2) NOT NULL,
    "operatingProfit" DECIMAL(15,2) NOT NULL,
    "totalAssets" DECIMAL(15,2) NOT NULL,
    "tangibleFixedAssets" DECIMAL(15,2) NOT NULL,
    "currentAssets" DECIMAL(15,2) NOT NULL,
    "currentLiabilities" DECIMAL(15,2) NOT NULL,
    "quickAssets" DECIMAL(15,2) NOT NULL,
    "equity" DECIMAL(15,2) NOT NULL,
    "operatingProfitMargin" DECIMAL(5,2) NOT NULL,
    "totalAssetTurnover" DECIMAL(5,2) NOT NULL,
    "tangibleFixedAssetTurnover" DECIMAL(5,2) NOT NULL,
    "currentRatio" DECIMAL(5,2) NOT NULL,
    "quickRatio" DECIMAL(5,2) NOT NULL,
    "equityRatio" DECIMAL(5,2) NOT NULL,
    "calculatedAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "financial_analysis_cache_pkey" PRIMARY KEY ("id")
);

-- CreateTable
CREATE TABLE "financial_trends" (
    "id" TEXT NOT NULL,
    "startFiscalYear" INTEGER NOT NULL,
    "endFiscalYear" INTEGER NOT NULL,
    "salesGrowthRate" DECIMAL(5,2) NOT NULL,
    "operatingProfitGrowthRate" DECIMAL(5,2) NOT NULL,
    "operatingProfitMarginChange" DECIMAL(5,2) NOT NULL,
    "totalAssetTurnoverChange" DECIMAL(5,2) NOT NULL,
    "equityRatioChange" DECIMAL(5,2) NOT NULL,
    "calculatedAt" TIMESTAMP(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
    "updatedAt" TIMESTAMP(3) NOT NULL,

    CONSTRAINT "financial_trends_pkey" PRIMARY KEY ("id")
);

-- CreateIndex
CREATE UNIQUE INDEX "financial_analysis_cache_fiscalYear_key" ON "financial_analysis_cache"("fiscalYear");

-- CreateIndex
CREATE INDEX "financial_analysis_cache_calculatedAt_idx" ON "financial_analysis_cache"("calculatedAt");

-- CreateIndex
CREATE UNIQUE INDEX "financial_trends_startFiscalYear_endFiscalYear_key" ON "financial_trends"("startFiscalYear", "endFiscalYear");
