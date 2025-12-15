# frozen_string_literal: true

class Account < ApplicationRecord
  # 勘定科目種別のenum定義
  enum :account_type, {
    asset: 0,       # 資産
    liability: 1,   # 負債
    equity: 2,      # 純資産
    revenue: 3,     # 収益
    expense: 4      # 費用
  }

  # バリデーション
  validates :code, presence: true, uniqueness: true
  validates :name, presence: true
  validates :account_type, presence: true
  validates :bspl_type, inclusion: { in: %w[B P], allow_nil: true }
  validates :debit_credit_type, inclusion: { in: %w[D C], allow_nil: true }

  # スコープ
  scope :balance_sheet, -> { where(bspl_type: 'B') }
  scope :profit_loss, -> { where(bspl_type: 'P') }
  scope :debit, -> { where(debit_credit_type: 'D') }
  scope :credit, -> { where(debit_credit_type: 'C') }
  scope :summary_accounts, -> { where(is_summary: true) }
  scope :detail_accounts, -> { where(is_summary: false) }

  # ヘルパーメソッド
  def balance_sheet?
    bspl_type == 'B'
  end

  def profit_loss?
    bspl_type == 'P'
  end

  def debit?
    debit_credit_type == 'D'
  end

  def credit?
    debit_credit_type == 'C'
  end

  def summary_account?
    is_summary
  end

  def detail_account?
    !is_summary
  end
end
