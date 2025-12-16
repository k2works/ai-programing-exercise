# frozen_string_literal: true

module Domain
  module Models
    class FinancialData
      attr_accessor :fiscal_year, :sales, :operating_profit, :total_assets,
                    :tangible_fixed_assets, :current_assets, :current_liabilities,
                    :quick_assets, :equity

      def initialize(fiscal_year:, sales:, operating_profit:, total_assets:,
                     tangible_fixed_assets:, current_assets:, current_liabilities:,
                     quick_assets:, equity:)
        @fiscal_year = fiscal_year
        @sales = BigDecimal(sales.to_s)
        @operating_profit = BigDecimal(operating_profit.to_s)
        @total_assets = BigDecimal(total_assets.to_s)
        @tangible_fixed_assets = BigDecimal(tangible_fixed_assets.to_s)
        @current_assets = BigDecimal(current_assets.to_s)
        @current_liabilities = BigDecimal(current_liabilities.to_s)
        @quick_assets = BigDecimal(quick_assets.to_s)
        @equity = BigDecimal(equity.to_s)
      end
    end
  end
end
