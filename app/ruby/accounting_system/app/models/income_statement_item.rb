# frozen_string_literal: true

# 損益計算書の項目（勘定科目ごとの金額と構成比率）
class IncomeStatementItem
  attr_accessor :account_code, :account_name, :balance, :percentage

  def initialize(account_code:, account_name:, balance:, percentage: nil)
    @account_code = account_code
    @account_name = account_name
    @balance = balance
    @percentage = percentage  # 対売上比
  end
end
