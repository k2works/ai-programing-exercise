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
  validates :debit_credit_type, inclusion: { in: %w[D C], allow_nil: true }

  # カスタムバリデーション
  validate :validate_bspl_type
  validate :validate_transaction_type
  validate :validate_expense_type

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

  private

  def validate_bspl_type
    return if bspl_type.blank?

    # BSPL区分の値チェック
    unless %w[B P].include?(bspl_type)
      errors.add(:bspl_type, 'は B または P のみ許可されています')
      return
    end

    # BSPL区分と勘定科目種別の整合性チェック
    if %w[asset liability equity].include?(account_type) && bspl_type != 'B'
      errors.add(:bspl_type, '資産・負債・純資産は B (貸借対照表) を設定してください')
    elsif %w[revenue expense].include?(account_type) && bspl_type != 'P'
      errors.add(:bspl_type, '収益・費用は P (損益計算書) を設定してください')
    end
  end

  def validate_transaction_type
    return if transaction_type.blank?

    # 取引要素区分の値チェック
    unless %w[1 2 3 4 5].include?(transaction_type)
      errors.add(:transaction_type, 'は 1〜5 のみ許可されています')
      return
    end

    # 取引要素区分と勘定科目種別の整合性チェック
    valid_combinations = {
      'asset' => '1',
      'liability' => '2',
      'equity' => '3',
      'revenue' => '4',
      'expense' => '5'
    }

    expected = valid_combinations[account_type]
    return unless expected && transaction_type != expected

    type_name = account_type_japanese_name
    errors.add(:transaction_type, "#{type_name}の取引要素区分は #{expected} である必要があります")
  end

  def account_type_japanese_name
    {
      'asset' => '資産',
      'liability' => '負債',
      'equity' => '純資産',
      'revenue' => '収益',
      'expense' => '費用'
    }[account_type]
  end

  def validate_expense_type
    return if expense_type.blank?

    # 費用区分の値チェック
    unless %w[1 2 3].include?(expense_type)
      errors.add(:expense_type, 'は 1〜3 のみ許可されています')
      return
    end

    # 費用区分は費用科目のみ設定可能
    return if account_type == 'expense'

    errors.add(:expense_type, 'は費用科目のみ設定可能です')
  end
end
