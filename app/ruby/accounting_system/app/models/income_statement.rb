# frozen_string_literal: true

# 損益計算書（Income Statement / P/L）
class IncomeStatement
  attr_accessor :from_date, :to_date, :revenues, :expenses,
                :gross_profit, :operating_income, :net_income,
                :total_revenues, :total_expenses

  def initialize(
    from_date:,
    to_date:,
    revenues: [],
    expenses: [],
    gross_profit: 0,
    operating_income: 0,
    net_income: 0,
    total_revenues: 0,
    total_expenses: 0
  )
    @from_date = from_date
    @to_date = to_date
    @revenues = revenues
    @expenses = expenses
    @gross_profit = gross_profit
    @operating_income = operating_income
    @net_income = net_income
    @total_revenues = total_revenues
    @total_expenses = total_expenses
  end
end
