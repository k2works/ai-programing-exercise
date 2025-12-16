# frozen_string_literal: true

# 財務指標計算サービス
class FinancialIndicatorService
  # すべての財務指標を計算
  #
  # @param balance_sheet [BalanceSheet] 貸借対照表
  # @param income_statement [IncomeStatement] 損益計算書
  # @return [Hash] 財務指標のハッシュ
  def self.calculate_all_indicators(balance_sheet, income_statement)
    {
      # 収益性指標
      gross_profit_margin: calculate_gross_profit_margin(income_statement),
      operating_profit_margin: calculate_operating_profit_margin(income_statement),

      # 効率性指標
      total_asset_turnover: calculate_total_asset_turnover(balance_sheet, income_statement),
      inventory_turnover: calculate_inventory_turnover(balance_sheet, income_statement),

      # 安全性指標
      current_ratio: calculate_current_ratio(balance_sheet),
      equity_ratio: calculate_equity_ratio(balance_sheet)
    }
  end

  # 売上高総利益率を計算
  #
  # @param income_statement [IncomeStatement] 損益計算書
  # @return [Float] 売上高総利益率（%）
  def self.calculate_gross_profit_margin(income_statement)
    return 0.0 if income_statement.total_revenues.zero?

    ((income_statement.gross_profit / income_statement.total_revenues) * 100).round(2)
  end

  # 売上高営業利益率を計算
  #
  # @param income_statement [IncomeStatement] 損益計算書
  # @return [Float] 売上高営業利益率（%）
  def self.calculate_operating_profit_margin(income_statement)
    return 0.0 if income_statement.total_revenues.zero?

    ((income_statement.operating_income / income_statement.total_revenues) * 100).round(2)
  end

  # 総資本回転率を計算
  #
  # @param balance_sheet [BalanceSheet] 貸借対照表
  # @param income_statement [IncomeStatement] 損益計算書
  # @return [Float] 総資本回転率（回）
  def self.calculate_total_asset_turnover(balance_sheet, income_statement)
    return 0.0 if balance_sheet.total_assets.zero?

    (income_statement.total_revenues / balance_sheet.total_assets).round(2)
  end

  # 棚卸資産回転率を計算
  #
  # @param balance_sheet [BalanceSheet] 貸借対照表
  # @param income_statement [IncomeStatement] 損益計算書
  # @return [Float] 棚卸資産回転率（回）
  def self.calculate_inventory_turnover(balance_sheet, income_statement)
    # 棚卸資産を取得（コードが1020で始まる資産）
    inventory = balance_sheet.assets
                             .select { |asset| asset.account_code.start_with?('1020') }
                             .sum(&:balance)

    return 0.0 if inventory.zero?

    # 売上原価を取得（コードが5001で始まる費用）
    cost_of_sales = income_statement.expenses
                                    .select { |expense| expense.account_code.start_with?('5001') }
                                    .sum(&:balance)

    (cost_of_sales / inventory).round(2)
  end

  # 流動比率を計算
  #
  # @param balance_sheet [BalanceSheet] 貸借対照表
  # @return [Float] 流動比率（%）
  def self.calculate_current_ratio(balance_sheet)
    # 流動資産（コードが10, 11, 12で始まる資産）
    current_assets = balance_sheet.assets
                                  .select { |asset| asset.account_code.start_with?('10', '11', '12') }
                                  .sum(&:balance)

    # 流動負債（コードが20, 21で始まる負債）
    current_liabilities = balance_sheet.liabilities
                                       .select { |liability| liability.account_code.start_with?('20', '21') }
                                       .sum(&:balance)

    return 0.0 if current_liabilities.zero?

    ((current_assets / current_liabilities) * 100).round(2)
  end

  # 自己資本比率を計算
  #
  # @param balance_sheet [BalanceSheet] 貸借対照表
  # @return [Float] 自己資本比率（%）
  def self.calculate_equity_ratio(balance_sheet)
    return 0.0 if balance_sheet.total_assets.zero?

    ((balance_sheet.total_equity / balance_sheet.total_assets) * 100).round(2)
  end
end
