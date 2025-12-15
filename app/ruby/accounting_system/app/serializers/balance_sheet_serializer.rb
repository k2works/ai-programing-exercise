# frozen_string_literal: true

# インフラストラクチャ層：レスポンス変換
# 貸借対照表のシリアライザ
class BalanceSheetSerializer
  def initialize(balance_sheet)
    @balance_sheet = balance_sheet
  end

  def as_json
    {
      as_of_date: @balance_sheet.as_of_date,
      assets: serialize_items(@balance_sheet.assets),
      liabilities: serialize_items(@balance_sheet.liabilities),
      equity: serialize_items(@balance_sheet.equity),
      total_assets: @balance_sheet.total_assets,
      total_liabilities: @balance_sheet.total_liabilities,
      total_equity: @balance_sheet.total_equity,
      total_liabilities_and_equity: @balance_sheet.total_liabilities_and_equity
    }
  end

  private

  def serialize_items(items)
    items.map do |item|
      {
        account_code: item.account_code,
        account_name: item.account_name,
        balance: item.balance,
        percentage: item.percentage
      }
    end
  end
end
