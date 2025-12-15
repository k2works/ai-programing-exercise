# frozen_string_literal: true

# 日次勘定科目残高モデル
# 日ごとの借方金額・貸方金額を記録する
class DailyAccountBalance < ApplicationRecord
  # アソシエーション
  belongs_to :account, foreign_key: :account_code, primary_key: :code

  # バリデーション
  validates :entry_date, presence: true
  validates :account_code, presence: true
  validates :settlement_flag, presence: true, inclusion: { in: [0, 1] }
  validates :debit_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
  validates :credit_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }

  # スコープ
  scope :by_date, ->(date) { where(entry_date: date) }
  scope :by_date_range, ->(start_date, end_date) { where(entry_date: start_date..end_date) }
  scope :by_account, ->(account_code) { where(account_code: account_code) }
  scope :normal, -> { where(settlement_flag: 0) }
  scope :settlement, -> { where(settlement_flag: 1) }
  scope :by_department, ->(department_code) { where(department_code: department_code) }
  scope :by_project, ->(project_code) { where(project_code: project_code) }

  # 残高を計算（借方 - 貸方）
  def balance
    debit_amount - credit_amount
  end
end
