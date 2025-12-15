# frozen_string_literal: true

require 'rails_helper'

RSpec.describe FinancialStatementService, type: :service do
  describe '貸借対照表の生成' do
    let(:as_of_date) { Date.new(2025, 1, 31) }

    before do
      # テーブルをクリア
      DailyAccountBalance.delete_all
      Account.delete_all

      # テストデータ：勘定科目マスタ（貸借対照表科目）
      # 資産
      Account.create!(
        code: '1110',
        name: '普通預金',
        account_type: :asset,
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1', # 資産
        display_order: 110
      )

      Account.create!(
        code: '1410',
        name: '建物',
        account_type: :asset,
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1', # 資産
        display_order: 140
      )

      # 負債
      Account.create!(
        code: '2110',
        name: '買掛金',
        account_type: :liability,
        bspl_type: 'B',
        debit_credit_type: 'C',
        transaction_type: '2', # 負債
        display_order: 210
      )

      Account.create!(
        code: '2510',
        name: '長期借入金',
        account_type: :liability,
        bspl_type: 'B',
        debit_credit_type: 'C',
        transaction_type: '2', # 負債
        display_order: 250
      )

      # 純資産
      Account.create!(
        code: '3110',
        name: '資本金',
        account_type: :equity,
        bspl_type: 'B',
        debit_credit_type: 'C',
        transaction_type: '3', # 純資産
        display_order: 310
      )

      # テストデータ：日次残高（2025-01-31時点）
      # 資産の残高は借方に計上
      DailyAccountBalance.create!(
        entry_date: as_of_date,
        account_code: '1110',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 8_000_000,
        credit_amount: 0
      )

      DailyAccountBalance.create!(
        entry_date: as_of_date,
        account_code: '1410',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 2_000_000,
        credit_amount: 0
      )

      # 負債の残高は貸方に計上
      DailyAccountBalance.create!(
        entry_date: as_of_date,
        account_code: '2110',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 0,
        credit_amount: 500_000
      )

      DailyAccountBalance.create!(
        entry_date: as_of_date,
        account_code: '2510',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 0,
        credit_amount: 4_500_000
      )

      # 純資産の残高は貸方に計上
      DailyAccountBalance.create!(
        entry_date: as_of_date,
        account_code: '3110',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 0,
        credit_amount: 5_000_000
      )
    end

    it '貸借対照表を生成できる' do
      # When: 貸借対照表を生成
      balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

      # Then: 貸借対照表のプロパティが存在する
      expect(balance_sheet).not_to be_nil
      expect(balance_sheet.as_of_date).to eq(as_of_date)
      expect(balance_sheet.assets).not_to be_empty
      expect(balance_sheet.liabilities).not_to be_empty
      expect(balance_sheet.equity).not_to be_empty
      expect(balance_sheet.total_assets).to be > 0
      expect(balance_sheet.total_liabilities).to be > 0
      expect(balance_sheet.total_equity).to be > 0
      expect(balance_sheet.total_liabilities_and_equity).to be > 0
    end

    it '貸借平均の原則が成立している' do
      # When: 貸借対照表を生成
      balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

      # Then: 資産 = 負債 + 純資産
      expected_total = balance_sheet.total_liabilities + balance_sheet.total_equity

      expect(balance_sheet.total_assets).to eq(expected_total)
      expect(balance_sheet.total_assets).to eq(balance_sheet.total_liabilities_and_equity)

      # 具体的な金額の検証
      # 資産: 普通預金8,000,000 + 建物2,000,000 = 10,000,000
      expect(balance_sheet.total_assets).to eq(10_000_000)

      # 負債: 買掛金500,000 + 長期借入金4,500,000 = 5,000,000
      expect(balance_sheet.total_liabilities).to eq(5_000_000)

      # 純資産: 資本金5,000,000
      expect(balance_sheet.total_equity).to eq(5_000_000)
    end

    it '資産項目が正しく分類されている' do
      # When: 貸借対照表を生成
      balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

      # Then: 資産項目が存在する
      assets = balance_sheet.assets
      expect(assets.size).to eq(2)

      # 流動資産（11で始まる勘定科目）
      current_assets = assets.select { |asset| asset.account_code.start_with?('11') }
      expect(current_assets.size).to eq(1)
      expect(current_assets[0].account_code).to eq('1110')
      expect(current_assets[0].account_name).to eq('普通預金')
      expect(current_assets[0].balance).to eq(8_000_000)

      # 固定資産（14で始まる勘定科目）
      fixed_assets = assets.select { |asset| asset.account_code.start_with?('14') }
      expect(fixed_assets.size).to eq(1)
      expect(fixed_assets[0].account_code).to eq('1410')
      expect(fixed_assets[0].account_name).to eq('建物')
      expect(fixed_assets[0].balance).to eq(2_000_000)
    end

    it '負債項目が正しく分類されている' do
      # When: 貸借対照表を生成
      balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

      # Then: 負債項目が存在する
      liabilities = balance_sheet.liabilities
      expect(liabilities.size).to eq(2)

      # 流動負債（21で始まる勘定科目）
      current_liabilities = liabilities.select { |liability| liability.account_code.start_with?('21') }
      expect(current_liabilities.size).to eq(1)
      expect(current_liabilities[0].account_code).to eq('2110')
      expect(current_liabilities[0].account_name).to eq('買掛金')
      expect(current_liabilities[0].balance).to eq(500_000)

      # 固定負債（25で始まる勘定科目）
      long_term_liabilities = liabilities.select { |liability| liability.account_code.start_with?('25') }
      expect(long_term_liabilities.size).to eq(1)
      expect(long_term_liabilities[0].account_code).to eq('2510')
      expect(long_term_liabilities[0].account_name).to eq('長期借入金')
      expect(long_term_liabilities[0].balance).to eq(4_500_000)
    end

    it '構成比率が計算されている' do
      # When: 貸借対照表を生成
      balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

      # Then: 各項目の構成比率が存在し、0〜100の範囲内
      balance_sheet.assets.each do |asset|
        expect(asset.percentage).not_to be_nil
        expect(asset.percentage).to be >= 0
        expect(asset.percentage).to be <= 100
      end

      balance_sheet.liabilities.each do |liability|
        expect(liability.percentage).not_to be_nil
        expect(liability.percentage).to be >= 0
        expect(liability.percentage).to be <= 100
      end

      balance_sheet.equity.each do |equity_item|
        expect(equity_item.percentage).not_to be_nil
        expect(equity_item.percentage).to be >= 0
        expect(equity_item.percentage).to be <= 100
      end

      # 普通預金の構成比率 = 8,000,000 / 10,000,000 × 100 = 80%
      ordinary_deposit = balance_sheet.assets.find { |asset| asset.account_code == '1110' }
      expect(ordinary_deposit.percentage).to eq(80.0)
    end

    it '純資産項目が正しく分類されている' do
      # When: 貸借対照表を生成
      balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

      # Then: 純資産項目が存在する
      equity = balance_sheet.equity
      expect(equity.size).to eq(1)
      expect(equity[0].account_code).to eq('3110')
      expect(equity[0].account_name).to eq('資本金')
      expect(equity[0].balance).to eq(5_000_000)
    end
  end

  describe '損益計算書の生成' do
    let(:from_date) { Date.new(2025, 1, 1) }
    let(:to_date) { Date.new(2025, 1, 31) }

    before do
      # テーブルをクリア
      DailyAccountBalance.delete_all
      Account.delete_all

      # テストデータ：勘定科目マスタ（損益計算書科目）
      # 収益
      Account.create!(
        code: '4010',
        name: '売上高',
        account_type: :revenue,
        bspl_type: 'P',
        debit_credit_type: 'C',
        transaction_type: '4', # 収益
        display_order: 410
      )

      # 費用（売上原価）
      Account.create!(
        code: '5110',
        name: '仕入高',
        account_type: :expense,
        bspl_type: 'P',
        debit_credit_type: 'D',
        transaction_type: '5', # 費用
        expense_type: '1', # 売上原価
        display_order: 510
      )

      # 費用（販管費）
      Account.create!(
        code: '6110',
        name: '給料手当',
        account_type: :expense,
        bspl_type: 'P',
        debit_credit_type: 'D',
        transaction_type: '5', # 費用
        expense_type: '2', # 販管費
        display_order: 610
      )

      # テストデータ：日次残高（2025年1月）
      # 収益は貸方に計上
      DailyAccountBalance.create!(
        entry_date: from_date,
        account_code: '4010',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 0,
        credit_amount: 10_000_000
      )

      # 費用は借方に計上
      DailyAccountBalance.create!(
        entry_date: from_date,
        account_code: '5110',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 6_000_000,
        credit_amount: 0
      )

      DailyAccountBalance.create!(
        entry_date: from_date,
        account_code: '6110',
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 2_000_000,
        credit_amount: 0
      )
    end

    it '損益計算書を生成できる' do
      # When: 損益計算書を生成
      income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)

      # Then: 損益計算書のプロパティが存在する
      expect(income_statement).not_to be_nil
      expect(income_statement.from_date).to eq(from_date)
      expect(income_statement.to_date).to eq(to_date)
      expect(income_statement.revenues).not_to be_empty
      expect(income_statement.expenses).not_to be_empty
      expect(income_statement.total_revenues).to be > 0
      expect(income_statement.total_expenses).to be > 0
    end

    it '利益項目が正しく計算されている' do
      # When: 損益計算書を生成
      income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)

      # Then: 利益項目が正しく計算されている
      # 売上高: 10,000,000
      expect(income_statement.total_revenues).to eq(10_000_000)

      # 売上原価: 6,000,000
      # 販管費: 2,000,000
      # 総費用: 8,000,000
      expect(income_statement.total_expenses).to eq(8_000_000)

      # 売上総利益 = 売上高 - 売上原価 = 10,000,000 - 6,000,000 = 4,000,000
      expect(income_statement.gross_profit).to eq(4_000_000)

      # 営業利益 = 売上総利益 - 販管費 = 4,000,000 - 2,000,000 = 2,000,000
      expect(income_statement.operating_income).to eq(2_000_000)

      # 当期純利益 = 収益 - 費用 = 10,000,000 - 8,000,000 = 2,000,000
      expect(income_statement.net_income).to eq(2_000_000)
    end

    it '収益項目が正しく分類されている' do
      # When: 損益計算書を生成
      income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)

      # Then: 収益項目が存在する
      revenues = income_statement.revenues
      expect(revenues.size).to eq(1)
      expect(revenues[0].account_code).to eq('4010')
      expect(revenues[0].account_name).to eq('売上高')
      expect(revenues[0].balance).to eq(10_000_000)
    end

    it '費用項目が正しく分類されている' do
      # When: 損益計算書を生成
      income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)

      # Then: 費用項目が存在する
      expenses = income_statement.expenses
      expect(expenses.size).to eq(2)

      # 売上原価
      cost_of_sales = expenses.find { |e| e.account_code == '5110' }
      expect(cost_of_sales).not_to be_nil
      expect(cost_of_sales.account_name).to eq('仕入高')
      expect(cost_of_sales.balance).to eq(6_000_000)

      # 販管費
      operating_expense = expenses.find { |e| e.account_code == '6110' }
      expect(operating_expense).not_to be_nil
      expect(operating_expense.account_name).to eq('給料手当')
      expect(operating_expense.balance).to eq(2_000_000)
    end

    it '対売上比が計算されている' do
      # When: 損益計算書を生成
      income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)

      # Then: 各項目の対売上比が存在し、正しく計算されている
      income_statement.revenues.each do |revenue|
        expect(revenue.percentage).not_to be_nil
        expect(revenue.percentage).to be >= 0
        expect(revenue.percentage).to be <= 100
      end

      income_statement.expenses.each do |expense|
        expect(expense.percentage).not_to be_nil
        expect(expense.percentage).to be >= 0
      end

      # 売上高の対売上比 = 10,000,000 / 10,000,000 × 100 = 100%
      sales = income_statement.revenues.find { |r| r.account_code == '4010' }
      expect(sales.percentage).to eq(100.0)

      # 仕入高の対売上比 = 6,000,000 / 10,000,000 × 100 = 60%
      purchases = income_statement.expenses.find { |e| e.account_code == '5110' }
      expect(purchases.percentage).to eq(60.0)
    end
  end
end
