# frozen_string_literal: true

require 'spec_helper'
require 'bigdecimal'

# ドメインモデルを明示的にロード
require_relative '../../../app/domain/models/financial_data'

RSpec.describe Domain::Models::FinancialData do
  describe '#initialize' do
    it '財務データを作成できる' do
      data = described_class.new(
        fiscal_year: 2023,
        sales: 400_000_000,
        operating_profit: 50_000_000,
        total_assets: 500_000_000,
        tangible_fixed_assets: 100_000_000,
        current_assets: 300_000_000,
        current_liabilities: 150_000_000,
        quick_assets: 200_000_000,
        equity: 250_000_000
      )

      expect(data.fiscal_year).to eq(2023)
      expect(data.sales).to eq(BigDecimal('400000000'))
      expect(data.operating_profit).to eq(BigDecimal('50000000'))
      expect(data.total_assets).to eq(BigDecimal('500000000'))
    end

    it 'すべての金額をBigDecimalに変換する' do
      data = described_class.new(
        fiscal_year: 2023,
        sales: '400000000',
        operating_profit: '50000000',
        total_assets: '500000000',
        tangible_fixed_assets: '100000000',
        current_assets: '300000000',
        current_liabilities: '150000000',
        quick_assets: '200000000',
        equity: '250000000'
      )

      expect(data.sales).to be_a(BigDecimal)
      expect(data.operating_profit).to be_a(BigDecimal)
    end
  end
end
