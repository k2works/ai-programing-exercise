# frozen_string_literal: true

# 貸借対照表の項目（勘定科目ごとの残高と構成比率）
class BalanceSheetItem
  attr_accessor :account_code, :account_name, :balance, :percentage

  def initialize(account_code:, account_name:, balance:, percentage: nil)
    @account_code = account_code
    @account_name = account_name
    @balance = balance
    @percentage = percentage
  end
end
