# frozen_string_literal: true

require 'spec_helper'
require 'bigdecimal'

# ドメインモデルを明示的にロード
require_relative '../../../app/domain/models/financial_data'
require_relative '../../../app/domain/models/financial_ratios'
require_relative '../../../app/application/services/analyze_financial_data_service'

RSpec.describe Application::Services::AnalyzeFinancialDataService do
  let(:mock_client) { double('FinancialAccountingClient') }
  let(:mock_cache_repository) { double('CacheRepository') }
  let(:service) { described_class.new(financial_client: mock_client, cache_repository: mock_cache_repository) }

  let(:financial_data) do
    Domain::Models::FinancialData.new(
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
  end

  describe '#execute' do
    it '財務データを取得して財務比率を計算する' do
      allow(mock_client).to receive(:fetch_financial_data_by_fiscal_year).with(2023).and_return(financial_data)
      allow(mock_cache_repository).to receive(:save).and_return(true)

      result = service.execute(2023)

      expect(result[:data]).to eq(financial_data)
      expect(result[:ratios]).to be_a(Domain::Models::FinancialRatios)

      # 営業利益率 = 営業利益 / 売上高 * 100
      expect(result[:ratios].operating_profit_margin).to eq(12.5)

      # 総資産回転率 = 売上高 / 総資産
      expect(result[:ratios].total_asset_turnover).to eq(0.8)

      # 有形固定資産回転率 = 売上高 / 有形固定資産
      expect(result[:ratios].tangible_fixed_asset_turnover).to eq(4.0)

      # 流動比率 = 流動資産 / 流動負債 * 100
      expect(result[:ratios].current_ratio).to eq(200.0)

      # 当座比率 = 当座資産 / 流動負債 * 100
      expect(result[:ratios].quick_ratio).to eq(133.33)

      # 自己資本比率 = 純資産 / 総資産 * 100
      expect(result[:ratios].equity_ratio).to eq(50.0)
    end
  end
end
