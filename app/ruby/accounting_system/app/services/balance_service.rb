# frozen_string_literal: true

# 残高更新サービス
# 仕訳登録時に日次残高を即時更新する
class BalanceService
  # 日次残高を更新（UPSERT）
  #
  # @param entry_date [Date] 起票日
  # @param account_code [String] 勘定科目コード
  # @param sub_account_code [String] 補助科目コード
  # @param department_code [String] 部門コード
  # @param project_code [String] プロジェクトコード
  # @param settlement_flag [Integer] 決算仕訳フラグ（0=通常、1=決算）
  # @param debit_amount [BigDecimal] 借方金額
  # @param credit_amount [BigDecimal] 貸方金額
  def self.update_daily_balance(
    entry_date:,
    account_code:,
    sub_account_code: '',
    department_code: '',
    project_code: '',
    settlement_flag: 0,
    debit_amount:,
    credit_amount:
  )
    # PostgreSQL の ON CONFLICT ... DO UPDATE を使用（UPSERT）
    sql = <<-SQL.squish
      INSERT INTO daily_account_balances (
        entry_date, account_code, sub_account_code, department_code,
        project_code, settlement_flag, debit_amount, credit_amount,
        created_at, updated_at
      ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)
      ON CONFLICT (entry_date, account_code, sub_account_code, department_code, project_code, settlement_flag)
      DO UPDATE SET
        debit_amount = daily_account_balances.debit_amount + EXCLUDED.debit_amount,
        credit_amount = daily_account_balances.credit_amount + EXCLUDED.credit_amount,
        updated_at = CURRENT_TIMESTAMP
    SQL

    ActiveRecord::Base.connection.execute(
      ActiveRecord::Base.sanitize_sql_array([
        sql,
        entry_date,
        account_code,
        sub_account_code,
        department_code,
        project_code,
        settlement_flag,
        debit_amount,
        credit_amount
      ])
    )
  end

  # 仕訳明細から残高を更新
  #
  # @param journal [Journal] 仕訳オブジェクト
  def self.update_balance_from_journal(journal)
    ActiveRecord::Base.transaction do
      journal.details.each do |detail|
        detail.items.each do |item|
          update_daily_balance(
            entry_date: journal.journal_date,
            account_code: item.account_code,
            sub_account_code: item.sub_account_code || '',
            department_code: item.department_code || '',
            project_code: item.project_code || '',
            settlement_flag: journal.settlement_flag,
            debit_amount: item.debit_credit_type == 'D' ? item.amount : 0,
            credit_amount: item.debit_credit_type == 'C' ? item.amount : 0
          )
        end
      end
    end
  end
end
