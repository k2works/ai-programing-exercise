-- ============================================
-- 管理会計サービス シードデータ
-- D社 令和3年度・令和4年度 財務分析結果
-- ============================================

-- 1. 財務分析結果キャッシュ（令和3年度）
INSERT INTO financial_analysis_cache (
    fiscal_year,
    sales, operating_profit, total_assets, tangible_fixed_assets,
    current_assets, current_liabilities, quick_assets, equity,
    operating_profit_margin, total_asset_turnover, tangible_asset_turnover,
    current_ratio, quick_ratio, equity_ratio,
    calculated_at, updated_at
) VALUES (
    2021,
    -- 財務データ
    5796105000.00,    -- 売上高
    985027000.00,     -- 営業利益
    2863166000.00,    -- 総資産
    64524000.00,      -- 有形固定資産（建物及び構築物）
    2676193000.00,    -- 流動資産
    851394000.00,     -- 流動負債
    1679096000.00,    -- 当座資産（現金預金 + 売掛金・受取手形）
    1989272000.00,    -- 自己資本
    -- 収益性指標
    16.99,            -- 営業利益率 (985,027 / 5,796,105)
    -- 効率性指標
    2.02,             -- 総資産回転率 (5,796,105 / 2,863,166)
    89.83,            -- 有形固定資産回転率 (5,796,105 / 64,524)
    -- 安全性指標
    314.33,           -- 流動比率 (2,676,193 / 851,394)
    197.22,           -- 当座比率 (1,679,096 / 851,394)
    69.48,            -- 自己資本比率 (1,989,272 / 2,863,166)
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- 2. 財務分析結果キャッシュ（令和4年度）
INSERT INTO financial_analysis_cache (
    fiscal_year,
    sales, operating_profit, total_assets, tangible_fixed_assets,
    current_assets, current_liabilities, quick_assets, equity,
    operating_profit_margin, total_asset_turnover, tangible_asset_turnover,
    current_ratio, quick_ratio, equity_ratio,
    calculated_at, updated_at
) VALUES (
    2022,
    -- 財務データ
    4547908000.00,    -- 売上高
    527037000.00,     -- 営業利益
    2974899000.00,    -- 総資産
    63256000.00,      -- 有形固定資産（建物及び構築物）
    2777545000.00,    -- 流動資産
    640513000.00,     -- 流動負債
    1998185000.00,    -- 当座資産（現金預金 + 売掛金）
    2307233000.00,    -- 自己資本
    -- 収益性指標
    11.59,            -- 営業利益率 (527,037 / 4,547,908)
    -- 効率性指標
    1.53,             -- 総資産回転率 (4,547,908 / 2,974,899)
    71.90,            -- 有形固定資産回転率 (4,547,908 / 63,256)
    -- 安全性指標
    433.64,           -- 流動比率 (2,777,545 / 640,513)
    311.97,           -- 当座比率 (1,998,185 / 640,513)
    77.56,            -- 自己資本比率 (2,307,233 / 2,974,899)
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

-- 3. 財務トレンド分析（令和3年度→令和4年度）
INSERT INTO financial_trend_analysis (
    start_fiscal_year, end_fiscal_year,
    sales_growth_rate, operating_profit_growth_rate,
    operating_profit_margin_change, total_asset_turnover_change, equity_ratio_change,
    calculated_at, updated_at
) VALUES (
    2021, 2022,
    -21.54,           -- 売上高成長率 ((4,547,908 - 5,796,105) / 5,796,105 * 100)
    -46.50,           -- 営業利益成長率 ((527,037 - 985,027) / 985,027 * 100)
    -5.40,            -- 営業利益率変化 (11.59 - 16.99)
    -0.49,            -- 総資産回転率変化 (1.53 - 2.02)
    8.08,             -- 自己資本比率変化 (77.56 - 69.48)
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
);

COMMENT ON TABLE financial_analysis_cache IS 'D社 令和3年度・令和4年度 財務分析結果';
COMMENT ON TABLE financial_trend_analysis IS 'D社 財務トレンド分析（令和3→4年度）';
