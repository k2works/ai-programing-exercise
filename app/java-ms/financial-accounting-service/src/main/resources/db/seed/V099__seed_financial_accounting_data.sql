-- ============================================
-- 財務会計サービス シードデータ
-- D社（化粧品製造販売会社）令和4年度データ
-- ============================================

-- 1. 勘定科目マスタのシードデータ
-- 貸借対照表項目（資産の部）
INSERT INTO accounts (account_code, account_name, account_type, bs_pl_classification, transaction_element, display_order, aggregation_target, balance) VALUES
('1110', '現金預金', '資産', 'B', '1', 10, TRUE, 1133270000.00),
('1120', '売掛金', '資産', 'B', '1', 20, TRUE, 864915000.00),
('1130', '受取手形', '資産', 'B', '1', 25, TRUE, 0.00),
('1140', '棚卸資産', '資産', 'B', '1', 30, TRUE, 740810000.00),
('1150', 'その他の流動資産', '資産', 'B', '1', 40, TRUE, 38550000.00),
('1210', '建物及び構築物', '資産', 'B', '1', 50, TRUE, 63256000.00),
('1220', '無形固定資産', '資産', 'B', '1', 60, TRUE, 34683000.00),
('1230', '投資その他の資産', '資産', 'B', '1', 70, TRUE, 99415000.00);

-- 貸借対照表項目（負債の部）
INSERT INTO accounts (account_code, account_name, account_type, bs_pl_classification, transaction_element, display_order, aggregation_target, balance) VALUES
('2110', '買掛金', '負債', 'B', '2', 110, TRUE, 197162000.00),
('2120', '短期借入金', '負債', 'B', '2', 120, TRUE, 70000000.00),
('2130', '未払金', '負債', 'B', '2', 130, TRUE, 104341000.00),
('2140', '未払法人税等', '負債', 'B', '2', 140, TRUE, 184887000.00),
('2150', 'その他の流動負債', '負債', 'B', '2', 150, TRUE, 84123000.00),
('2210', '長期借入金', '負債', 'B', '2', 160, TRUE, 24360000.00),
('2220', 'リース債務', '負債', 'B', '2', 170, TRUE, 2793000.00);

-- 貸借対照表項目（純資産の部）
INSERT INTO accounts (account_code, account_name, account_type, bs_pl_classification, transaction_element, display_order, aggregation_target, balance) VALUES
('3110', '資本金', '純資産', 'B', '3', 210, TRUE, 100000000.00),
('3210', '利益剰余金', '純資産', 'B', '3', 220, TRUE, 2207233000.00);

-- 損益計算書項目（収益）
INSERT INTO accounts (account_code, account_name, account_type, bs_pl_classification, transaction_element, display_order, aggregation_target, balance) VALUES
('4110', '売上高', '収益', 'P', '5', 310, TRUE, 0.00),
('4210', '営業外収益', '収益', 'P', '5', 320, TRUE, 0.00);

-- 損益計算書項目（費用）
INSERT INTO accounts (account_code, account_name, account_type, bs_pl_classification, transaction_element, expense_category, display_order, aggregation_target, balance) VALUES
('5110', '売上原価', '費用', 'P', '4', '売上原価', 410, TRUE, 0.00),
('5210', '販売費及び一般管理費', '費用', 'P', '4', '販管費', 420, TRUE, 0.00),
('5310', '営業外費用', '費用', 'P', '4', '営業外', 430, TRUE, 0.00),
('5410', '法人税等', '費用', 'P', '4', '税金', 440, TRUE, 0.00);

-- 2. 仕訳エントリと仕訳明細のシードデータ（令和4年度決算仕訳）
-- 売上と売上原価の計上
INSERT INTO journal_headers (voucher_number, journal_date, description, total_amount, reference_number, created_by, created_at, updated_by, updated_at) VALUES
('R04-0001', '2023-03-31', '令和4年度売上計上', 4547908000.00, 'R04-FS', 'system', CURRENT_TIMESTAMP, 'system', CURRENT_TIMESTAMP),
('R04-0002', '2023-03-31', '令和4年度売上原価計上', 1743821000.00, 'R04-FS', 'system', CURRENT_TIMESTAMP, 'system', CURRENT_TIMESTAMP),
('R04-0003', '2023-03-31', '令和4年度販管費計上', 2277050000.00, 'R04-FS', 'system', CURRENT_TIMESTAMP, 'system', CURRENT_TIMESTAMP),
('R04-0004', '2023-03-31', '令和4年度営業外損益', 9995000.00, 'R04-FS', 'system', CURRENT_TIMESTAMP, 'system', CURRENT_TIMESTAMP),
('R04-0005', '2023-03-31', '令和4年度法人税等', 169072000.00, 'R04-FS', 'system', CURRENT_TIMESTAMP, 'system', CURRENT_TIMESTAMP);

-- 仕訳明細（売上）
INSERT INTO journal_details (voucher_number, line_number, account_code, debit_amount, credit_amount, description) VALUES
('R04-0001', 1, '1120', 4547908000.00, 0.00, 'D社 令和4年度売上'),
('R04-0001', 2, '4110', 0.00, 4547908000.00, 'D社 令和4年度売上');

-- 仕訳明細（売上原価）
INSERT INTO journal_details (voucher_number, line_number, account_code, debit_amount, credit_amount, description) VALUES
('R04-0002', 1, '5110', 1743821000.00, 0.00, '令和4年度売上原価'),
('R04-0002', 2, '1140', 0.00, 1743821000.00, '棚卸資産減少');

-- 仕訳明細（販管費）
INSERT INTO journal_details (voucher_number, line_number, account_code, debit_amount, credit_amount, description) VALUES
('R04-0003', 1, '5210', 2277050000.00, 0.00, '令和4年度販管費'),
('R04-0003', 2, '2130', 0.00, 2277050000.00, '未払金計上');

-- 仕訳明細（営業外損益：営業外収益11,608千円 - 営業外費用1,613千円 = 9,995千円）
INSERT INTO journal_details (voucher_number, line_number, account_code, debit_amount, credit_amount, description) VALUES
('R04-0004', 1, '1110', 9995000.00, 0.00, '営業外収益（純額）'),
('R04-0004', 2, '4210', 0.00, 9995000.00, '営業外収益計上');

-- 仕訳明細（法人税等）
INSERT INTO journal_details (voucher_number, line_number, account_code, debit_amount, credit_amount, description) VALUES
('R04-0005', 1, '5410', 169072000.00, 0.00, '令和4年度法人税等'),
('R04-0005', 2, '2140', 0.00, 169072000.00, '未払法人税等');

-- 3. 日次勘定科目残高のシードデータ（期末残高）
INSERT INTO daily_account_balances (balance_date, account_code, sub_account_code, department_code, project_code, settlement_flag, debit_amount, credit_amount) VALUES
('2023-03-31', '1110', '', '', '', 1, 1133270000.00, 0.00),  -- 現金預金
('2023-03-31', '1120', '', '', '', 1, 864915000.00, 0.00),   -- 売掛金
('2023-03-31', '1140', '', '', '', 1, 740810000.00, 0.00),   -- 棚卸資産
('2023-03-31', '2110', '', '', '', 1, 0.00, 197162000.00),   -- 買掛金
('2023-03-31', '2120', '', '', '', 1, 0.00, 70000000.00),    -- 短期借入金
('2023-03-31', '3110', '', '', '', 1, 0.00, 100000000.00),   -- 資本金
('2023-03-31', '3210', '', '', '', 1, 0.00, 2207233000.00),  -- 利益剰余金
('2023-03-31', '4110', '', '', '', 1, 0.00, 4547908000.00),  -- 売上高
('2023-03-31', '5110', '', '', '', 1, 1743821000.00, 0.00),  -- 売上原価
('2023-03-31', '5210', '', '', '', 1, 2277050000.00, 0.00);  -- 販管費

COMMENT ON TABLE accounts IS 'D社（化粧品製造販売）令和4年度財務データ';
