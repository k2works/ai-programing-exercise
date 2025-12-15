# frozen_string_literal: true

# 月次勘定科目残高モデル
# 月ごとの月初残高・借方金額・貸方金額・月末残高を記録する
class MonthlyAccountBalance < ApplicationRecord
  # アソシエーション
  belongs_to :account, foreign_key: :account_code, primary_key: :code

  # バリデーション
  validates :fiscal_year, presence: true, numericality: { only_integer: true, greater_than: 0 }
  validates :month, presence: true, inclusion: { in: 1..12 }
  validates :account_code, presence: true, length: { maximum: 10 }
  validates :sub_account_code, length: { maximum: 10 }
  validates :department_code, length: { maximum: 5 }
  validates :project_code, length: { maximum: 10 }
  validates :settlement_flag, inclusion: { in: [0, 1] }
  validates :beginning_balance, numericality: true
  validates :debit_amount, numericality: { greater_than_or_equal_to: 0 }
  validates :credit_amount, numericality: { greater_than_or_equal_to: 0 }
  validates :ending_balance, numericality: true

  # スコープ
  scope :normal_entries, -> { where(settlement_flag: 0) }
  scope :settlement_entries, -> { where(settlement_flag: 1) }
  scope :by_fiscal_year, ->(year) { where(fiscal_year: year) }
  scope :by_month, ->(month) { where(month: month) }
  scope :by_account, ->(account_code) { where(account_code: account_code) }
  scope :by_department, ->(department_code) { where(department_code: department_code) }
  scope :by_project, ->(project_code) { where(project_code: project_code) }

  # 月末残高を計算（月初残高 + 借方金額 - 貸方金額）
  def calculate_ending_balance
    beginning_balance + debit_amount - credit_amount
  end
end
