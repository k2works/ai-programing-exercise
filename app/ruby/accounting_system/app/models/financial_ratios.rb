# frozen_string_literal: true

# 財務指標
class FinancialRatios
  attr_accessor :current_ratio,           # 流動比率（％）
                :debt_to_equity_ratio,    # 自己資本比率（％）
                :gross_profit_margin,     # 売上総利益率（％）
                :operating_profit_margin, # 営業利益率（％）
                :net_profit_margin,       # 当期純利益率（％）
                :roa,                     # 総資産利益率（ROA, ％）
                :roe                      # 自己資本利益率（ROE, ％）

  def initialize(
    current_ratio: 0.0,
    debt_to_equity_ratio: 0.0,
    gross_profit_margin: 0.0,
    operating_profit_margin: 0.0,
    net_profit_margin: 0.0,
    roa: 0.0,
    roe: 0.0
  )
    @current_ratio = current_ratio
    @debt_to_equity_ratio = debt_to_equity_ratio
    @gross_profit_margin = gross_profit_margin
    @operating_profit_margin = operating_profit_margin
    @net_profit_margin = net_profit_margin
    @roa = roa
    @roe = roe
  end
end
