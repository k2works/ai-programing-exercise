# frozen_string_literal: true

require 'spec_helper'

# ドメインモデルを明示的にロード
require_relative '../../../app/domain/models/financial_ratios'

RSpec.describe Domain::Models::FinancialRatios do
  describe '#initialize' do
    it '財務比率を作成できる' do
      ratios = described_class.new(
        operating_profit_margin: 12.5,
        total_asset_turnover: 0.8,
        tangible_fixed_asset_turnover: 4.0,
        current_ratio: 200.0,
        quick_ratio: 133.33,
        equity_ratio: 50.0
      )

      expect(ratios.operating_profit_margin).to eq(12.5)
      expect(ratios.total_asset_turnover).to eq(0.8)
      expect(ratios.tangible_fixed_asset_turnover).to eq(4.0)
      expect(ratios.current_ratio).to eq(200.0)
      expect(ratios.quick_ratio).to eq(133.33)
      expect(ratios.equity_ratio).to eq(50.0)
    end
  end
end
