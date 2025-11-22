-- ==========================================
-- Financial Analysis Tables
-- ==========================================

-- 財務分析結果キャッシュ (Financial Analysis Result Cache)
CREATE TABLE IF NOT EXISTS financial_analysis_cache (
    fiscal_year INTEGER PRIMARY KEY,
    -- 財務データ
    sales NUMERIC(15,2) NOT NULL,                      -- 売上高
    operating_profit NUMERIC(15,2) NOT NULL,           -- 営業利益
    total_assets NUMERIC(15,2) NOT NULL,               -- 総資産
    tangible_fixed_assets NUMERIC(15,2) NOT NULL,      -- 有形固定資産
    current_assets NUMERIC(15,2) NOT NULL,             -- 流動資産
    current_liabilities NUMERIC(15,2) NOT NULL,        -- 流動負債
    quick_assets NUMERIC(15,2) NOT NULL,               -- 当座資産
    equity NUMERIC(15,2) NOT NULL,                     -- 自己資本
    -- 収益性指標
    operating_profit_margin NUMERIC(5,2) NOT NULL,     -- 営業利益率
    -- 効率性指標
    total_asset_turnover NUMERIC(5,2) NOT NULL,        -- 総資産回転率
    tangible_asset_turnover NUMERIC(5,2) NOT NULL,     -- 有形固定資産回転率
    -- 安全性指標
    current_ratio NUMERIC(5,2) NOT NULL,               -- 流動比率
    quick_ratio NUMERIC(5,2) NOT NULL,                 -- 当座比率
    equity_ratio NUMERIC(5,2) NOT NULL,                -- 自己資本比率
    -- メタデータ
    calculated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL
);

CREATE INDEX idx_analysis_cache_year ON financial_analysis_cache(fiscal_year);

-- 財務トレンド分析 (Financial Trend Analysis)
CREATE TABLE IF NOT EXISTS financial_trend_analysis (
    start_fiscal_year INTEGER NOT NULL,
    end_fiscal_year INTEGER NOT NULL,
    -- 成長率
    sales_growth_rate NUMERIC(5,2) NOT NULL,           -- 売上高成長率
    operating_profit_growth_rate NUMERIC(5,2) NOT NULL, -- 営業利益成長率
    -- 変化率
    operating_profit_margin_change NUMERIC(5,2) NOT NULL, -- 営業利益率変化
    total_asset_turnover_change NUMERIC(5,2) NOT NULL,    -- 総資産回転率変化
    equity_ratio_change NUMERIC(5,2) NOT NULL,            -- 自己資本比率変化
    -- メタデータ
    calculated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,
    PRIMARY KEY (start_fiscal_year, end_fiscal_year)
);

CREATE INDEX idx_trend_start_year ON financial_trend_analysis(start_fiscal_year);
CREATE INDEX idx_trend_end_year ON financial_trend_analysis(end_fiscal_year);

-- Comments
COMMENT ON TABLE financial_analysis_cache IS '財務分析結果キャッシュ';
COMMENT ON TABLE financial_trend_analysis IS '財務トレンド分析';

COMMENT ON COLUMN financial_analysis_cache.fiscal_year IS '会計年度';
COMMENT ON COLUMN financial_analysis_cache.sales IS '売上高';
COMMENT ON COLUMN financial_analysis_cache.operating_profit IS '営業利益';
COMMENT ON COLUMN financial_analysis_cache.total_assets IS '総資産';
COMMENT ON COLUMN financial_analysis_cache.tangible_fixed_assets IS '有形固定資産';
COMMENT ON COLUMN financial_analysis_cache.current_assets IS '流動資産';
COMMENT ON COLUMN financial_analysis_cache.current_liabilities IS '流動負債';
COMMENT ON COLUMN financial_analysis_cache.quick_assets IS '当座資産';
COMMENT ON COLUMN financial_analysis_cache.equity IS '自己資本';
COMMENT ON COLUMN financial_analysis_cache.operating_profit_margin IS '営業利益率（%）';
COMMENT ON COLUMN financial_analysis_cache.total_asset_turnover IS '総資産回転率（回）';
COMMENT ON COLUMN financial_analysis_cache.tangible_asset_turnover IS '有形固定資産回転率（回）';
COMMENT ON COLUMN financial_analysis_cache.current_ratio IS '流動比率（%）';
COMMENT ON COLUMN financial_analysis_cache.quick_ratio IS '当座比率（%）';
COMMENT ON COLUMN financial_analysis_cache.equity_ratio IS '自己資本比率（%）';

COMMENT ON COLUMN financial_trend_analysis.start_fiscal_year IS '開始会計年度';
COMMENT ON COLUMN financial_trend_analysis.end_fiscal_year IS '終了会計年度';
COMMENT ON COLUMN financial_trend_analysis.sales_growth_rate IS '売上高成長率（%）';
COMMENT ON COLUMN financial_trend_analysis.operating_profit_growth_rate IS '営業利益成長率（%）';
COMMENT ON COLUMN financial_trend_analysis.operating_profit_margin_change IS '営業利益率変化（ポイント）';
COMMENT ON COLUMN financial_trend_analysis.total_asset_turnover_change IS '総資産回転率変化（回）';
COMMENT ON COLUMN financial_trend_analysis.equity_ratio_change IS '自己資本比率変化（ポイント）';
