# frozen_string_literal: true

module Domain
  module Models
    class FinancialRatios
      attr_accessor :operating_profit_margin, :total_asset_turnover,
                    :tangible_fixed_asset_turnover, :current_ratio,
                    :quick_ratio, :equity_ratio

      def initialize(operating_profit_margin:, total_asset_turnover:,
                     tangible_fixed_asset_turnover:, current_ratio:,
                     quick_ratio:, equity_ratio:)
        @operating_profit_margin = operating_profit_margin
        @total_asset_turnover = total_asset_turnover
        @tangible_fixed_asset_turnover = tangible_fixed_asset_turnover
        @current_ratio = current_ratio
        @quick_ratio = quick_ratio
        @equity_ratio = equity_ratio
      end
    end
  end
end
