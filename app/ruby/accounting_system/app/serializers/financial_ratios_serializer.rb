# frozen_string_literal: true

# インフラストラクチャ層:レスポンス変換
# 財務指標のシリアライザ
class FinancialRatiosSerializer
  def initialize(financial_ratios)
    @financial_ratios = financial_ratios
  end

  def as_json
    {
      current_ratio: @financial_ratios.current_ratio,
      debt_to_equity_ratio: @financial_ratios.debt_to_equity_ratio,
      gross_profit_margin: @financial_ratios.gross_profit_margin,
      operating_profit_margin: @financial_ratios.operating_profit_margin,
      net_profit_margin: @financial_ratios.net_profit_margin,
      roa: @financial_ratios.roa,
      roe: @financial_ratios.roe
    }
  end
end
