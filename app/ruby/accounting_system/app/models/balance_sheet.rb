# frozen_string_literal: true

# 貸借対照表（Balance Sheet / B/S）
class BalanceSheet
  attr_accessor :as_of_date, :assets, :liabilities, :equity,
                :total_assets, :total_liabilities, :total_equity,
                :total_liabilities_and_equity

  def initialize(
    as_of_date:,
    assets: [],
    liabilities: [],
    equity: [],
    total_assets: 0,
    total_liabilities: 0,
    total_equity: 0,
    total_liabilities_and_equity: 0
  )
    @as_of_date = as_of_date
    @assets = assets
    @liabilities = liabilities
    @equity = equity
    @total_assets = total_assets
    @total_liabilities = total_liabilities
    @total_equity = total_equity
    @total_liabilities_and_equity = total_liabilities_and_equity
  end
end
