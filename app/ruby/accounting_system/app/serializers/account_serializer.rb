# frozen_string_literal: true

# インフラストラクチャ層：レスポンス変換
# 勘定科目のシリアライザ
class AccountSerializer
  def initialize(account)
    @account = account
  end

  def as_json
    {
      code: @account.code,
      name: @account.name,
      account_type: @account.account_type,
      bspl_type: @account.bspl_type,
      debit_credit_type: @account.debit_credit_type,
      transaction_type: @account.transaction_type,
      expense_type: @account.expense_type,
      tax_code: @account.tax_code,
      display_order: @account.display_order,
      is_summary: @account.is_summary
    }
  end
end
