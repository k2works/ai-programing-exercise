# frozen_string_literal: true

module Application
  module Services
    class AnalyzeFinancialDataService
      def initialize(financial_client:, cache_repository:)
        @financial_client = financial_client
        @cache_repository = cache_repository
      end

      def execute(fiscal_year)
        # 1. 財務会計サービスからデータ取得
        data = @financial_client.fetch_financial_data_by_fiscal_year(fiscal_year)

        # 2. 財務比率の計算
        ratios = calculate_ratios(data)

        # 3. 結果をキャッシュ
        cache = @cache_repository.save(data, ratios)

        { data: data, ratios: ratios, cache: cache }
      end

      private

      def calculate_ratios(data)
        Domain::Models::FinancialRatios.new(
          operating_profit_margin: calculate_operating_profit_margin(data),
          total_asset_turnover: calculate_total_asset_turnover(data),
          tangible_fixed_asset_turnover: calculate_tangible_fixed_asset_turnover(data),
          current_ratio: calculate_current_ratio(data),
          quick_ratio: calculate_quick_ratio(data),
          equity_ratio: calculate_equity_ratio(data)
        )
      end

      def calculate_operating_profit_margin(data)
        return 0 if data.sales.zero?
        (data.operating_profit / data.sales * 100).round(2).to_f
      end

      def calculate_total_asset_turnover(data)
        return 0 if data.total_assets.zero?
        (data.sales / data.total_assets).round(2).to_f
      end

      def calculate_tangible_fixed_asset_turnover(data)
        return 0 if data.tangible_fixed_assets.zero?
        (data.sales / data.tangible_fixed_assets).round(2).to_f
      end

      def calculate_current_ratio(data)
        return 0 if data.current_liabilities.zero?
        (data.current_assets / data.current_liabilities * 100).round(2).to_f
      end

      def calculate_quick_ratio(data)
        return 0 if data.current_liabilities.zero?
        (data.quick_assets / data.current_liabilities * 100).round(2).to_f
      end

      def calculate_equity_ratio(data)
        return 0 if data.total_assets.zero?
        (data.equity / data.total_assets * 100).round(2).to_f
      end
    end
  end
end
