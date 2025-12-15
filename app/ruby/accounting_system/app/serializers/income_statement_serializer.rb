# frozen_string_literal: true

# インフラストラクチャ層：レスポンス変換
# 損益計算書のシリアライザ
class IncomeStatementSerializer
  def initialize(income_statement)
    @income_statement = income_statement
  end

  def as_json
    {
      from_date: @income_statement.from_date,
      to_date: @income_statement.to_date,
      revenues: serialize_items(@income_statement.revenues),
      expenses: serialize_items(@income_statement.expenses),
      gross_profit: @income_statement.gross_profit.to_f,
      operating_income: @income_statement.operating_income.to_f,
      net_income: @income_statement.net_income.to_f,
      total_revenues: @income_statement.total_revenues.to_f,
      total_expenses: @income_statement.total_expenses.to_f
    }
  end

  private

  def serialize_items(items)
    items.map do |item|
      {
        account_code: item.account_code,
        account_name: item.account_name,
        balance: item.balance.to_f,
        percentage: item.percentage.to_f
      }
    end
  end
end
