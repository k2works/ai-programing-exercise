# frozen_string_literal: true

require 'rails_helper'

RSpec.describe FinancialIndicatorService do
  let(:balance_sheet) do
    BalanceSheet.new(
      as_of_date: Date.parse('2023-03-31'),
      assets: [
        BalanceSheetItem.new(account_code: '1001', account_name: '現金預金', balance: 1_158_270_000),
        BalanceSheetItem.new(account_code: '1010', account_name: '売掛金', balance: 989_915_000),
        BalanceSheetItem.new(account_code: '1020', account_name: '棚卸資産', balance: 740_810_000),
        BalanceSheetItem.new(account_code: '1100', account_name: '固定資産', balance: 197_354_000)
      ],
      liabilities: [
        BalanceSheetItem.new(account_code: '2001', account_name: '買掛金', balance: 247_162_000),
        BalanceSheetItem.new(account_code: '2010', account_name: '短期借入金', balance: 70_000_000)
      ],
      equity: [
        BalanceSheetItem.new(account_code: '3001', account_name: '資本金', balance: 100_000_000),
        BalanceSheetItem.new(account_code: '3010', account_name: '利益剰余金', balance: 2_669_187_000)
      ],
      total_assets: 3_086_349_000,
      total_liabilities: 317_162_000,
      total_equity: 2_769_187_000,
      total_liabilities_and_equity: 3_086_349_000
    )
  end

  let(:income_statement) do
    IncomeStatement.new(
      from_date: Date.parse('2022-04-01'),
      to_date: Date.parse('2023-03-31'),
      revenues: [
        IncomeStatementItem.new(account_code: '4001', account_name: '売上高', balance: 150_000_000)
      ],
      expenses: [
        IncomeStatementItem.new(account_code: '5001', account_name: '売上原価', balance: 50_000_000),
        IncomeStatementItem.new(account_code: '5010', account_name: '販売費及び一般管理費', balance: 35_000_000)
      ],
      gross_profit: 100_000_000,
      operating_income: 65_000_000,
      net_income: 65_000_000,
      total_revenues: 150_000_000,
      total_expenses: 85_000_000
    )
  end

  describe '.calculate_all_indicators' do
    it 'すべての財務指標を計算できる' do
      indicators = described_class.calculate_all_indicators(balance_sheet, income_statement)

      expect(indicators).to include(
        gross_profit_margin: be_a(Float),
        operating_profit_margin: be_a(Float),
        total_asset_turnover: be_a(Float),
        inventory_turnover: be_a(Float),
        current_ratio: be_a(Float),
        equity_ratio: be_a(Float)
      )
    end
  end

  describe '.calculate_gross_profit_margin' do
    it '売上高総利益率を計算できる' do
      result = described_class.calculate_gross_profit_margin(income_statement)
      expect(result).to eq(66.67) # (100_000_000 / 150_000_000) * 100 = 66.67%
    end

    context '売上高が0の場合' do
      let(:income_statement_zero) do
        IncomeStatement.new(
          from_date: Date.parse('2022-04-01'),
          to_date: Date.parse('2023-03-31'),
          revenues: [],
          expenses: [],
          gross_profit: 0,
          operating_income: 0,
          net_income: 0,
          total_revenues: 0,
          total_expenses: 0
        )
      end

      it '0を返す' do
        result = described_class.calculate_gross_profit_margin(income_statement_zero)
        expect(result).to eq(0.0)
      end
    end
  end

  describe '.calculate_operating_profit_margin' do
    it '売上高営業利益率を計算できる' do
      result = described_class.calculate_operating_profit_margin(income_statement)
      expect(result).to eq(43.33) # (65_000_000 / 150_000_000) * 100 = 43.33%
    end
  end

  describe '.calculate_total_asset_turnover' do
    it '総資本回転率を計算できる' do
      result = described_class.calculate_total_asset_turnover(balance_sheet, income_statement)
      expect(result).to eq(0.05) # 150_000_000 / 3_086_349_000 = 0.05回
    end

    context '総資産が0の場合' do
      let(:balance_sheet_zero) do
        BalanceSheet.new(
          as_of_date: Date.parse('2023-03-31'),
          assets: [],
          liabilities: [],
          equity: [],
          total_assets: 0,
          total_liabilities: 0,
          total_equity: 0,
          total_liabilities_and_equity: 0
        )
      end

      it '0を返す' do
        result = described_class.calculate_total_asset_turnover(balance_sheet_zero, income_statement)
        expect(result).to eq(0.0)
      end
    end
  end

  describe '.calculate_inventory_turnover' do
    it '棚卸資産回転率を計算できる' do
      result = described_class.calculate_inventory_turnover(balance_sheet, income_statement)
      expect(result).to eq(0.07) # 50_000_000 / 740_810_000 = 0.07回
    end

    context '棚卸資産が0の場合' do
      let(:balance_sheet_no_inventory) do
        BalanceSheet.new(
          as_of_date: Date.parse('2023-03-31'),
          assets: [
            BalanceSheetItem.new(account_code: '1001', account_name: '現金預金', balance: 1_000_000)
          ],
          liabilities: [],
          equity: [],
          total_assets: 1_000_000,
          total_liabilities: 0,
          total_equity: 1_000_000,
          total_liabilities_and_equity: 1_000_000
        )
      end

      it '0を返す' do
        result = described_class.calculate_inventory_turnover(balance_sheet_no_inventory, income_statement)
        expect(result).to eq(0.0)
      end
    end
  end

  describe '.calculate_current_ratio' do
    it '流動比率を計算できる' do
      result = described_class.calculate_current_ratio(balance_sheet)
      # 流動資産: 1_158_270_000 + 989_915_000 + 740_810_000 = 2_888_995_000
      # 流動負債: 247_162_000 + 70_000_000 = 317_162_000
      # (2_888_995_000 / 317_162_000) * 100 = 911.16%
      expect(result).to eq(911.16)
    end

    context '流動負債が0の場合' do
      let(:balance_sheet_no_liabilities) do
        BalanceSheet.new(
          as_of_date: Date.parse('2023-03-31'),
          assets: [
            BalanceSheetItem.new(account_code: '1001', account_name: '現金預金', balance: 1_000_000)
          ],
          liabilities: [],
          equity: [],
          total_assets: 1_000_000,
          total_liabilities: 0,
          total_equity: 1_000_000,
          total_liabilities_and_equity: 1_000_000
        )
      end

      it '0を返す' do
        result = described_class.calculate_current_ratio(balance_sheet_no_liabilities)
        expect(result).to eq(0.0)
      end
    end
  end

  describe '.calculate_equity_ratio' do
    it '自己資本比率を計算できる' do
      result = described_class.calculate_equity_ratio(balance_sheet)
      expect(result).to eq(89.72) # (2_769_187_000 / 3_086_349_000) * 100 = 89.72%
    end

    context '総資産が0の場合' do
      let(:balance_sheet_zero) do
        BalanceSheet.new(
          as_of_date: Date.parse('2023-03-31'),
          assets: [],
          liabilities: [],
          equity: [],
          total_assets: 0,
          total_liabilities: 0,
          total_equity: 0,
          total_liabilities_and_equity: 0
        )
      end

      it '0を返す' do
        result = described_class.calculate_equity_ratio(balance_sheet_zero)
        expect(result).to eq(0.0)
      end
    end
  end
end
