# frozen_string_literal: true

namespace :financial do
  desc 'D社の財務分析レポートを生成'
  task analyze: :environment do
    as_of_date = Date.parse('2023-03-31') # 令和4年度末

    # 貸借対照表生成
    balance_sheet = FinancialStatementService.generate_balance_sheet(as_of_date)

    # 損益計算書生成
    from_date = Date.parse('2022-04-01')
    to_date = Date.parse('2023-03-31')
    income_statement = FinancialStatementService.generate_income_statement(from_date, to_date)

    # 財務指標計算
    indicators = FinancialIndicatorService.calculate_all_indicators(balance_sheet, income_statement)

    # レポート出力
    puts '=' * 60
    puts 'D社 令和4年度 財務分析レポート'
    puts '=' * 60
    puts
    puts '【収益性】'
    puts "売上高総利益率: #{indicators[:gross_profit_margin]}%"
    puts "売上高営業利益率: #{indicators[:operating_profit_margin]}%"
    puts
    puts '【効率性】'
    puts "総資本回転率: #{indicators[:total_asset_turnover]}回"
    puts "棚卸資産回転率: #{indicators[:inventory_turnover]}回"
    puts
    puts '【安全性】'
    puts "流動比率: #{indicators[:current_ratio]}%"
    puts "自己資本比率: #{indicators[:equity_ratio]}%"
  end
end
