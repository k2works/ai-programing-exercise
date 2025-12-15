# frozen_string_literal: true

# 仕訳明細モデル
# 仕訳エントリの明細行データを管理する
class JournalEntryDetail < ApplicationRecord
  # アソシエーション
  belongs_to :journal_entry
  belongs_to :account, foreign_key: :account_code, primary_key: :code

  # バリデーション
  validates :line_number, presence: true,
                          uniqueness: { scope: :journal_entry_id }
  validates :account_code, presence: true
  validates :debit_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
  validates :credit_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
  validates :description, presence: true

  # カスタムバリデーション：借方と貸方の両方に金額を設定できない
  validate :debit_or_credit_not_both

  # スコープ
  scope :debits, -> { where('debit_amount > 0') }
  scope :credits, -> { where('credit_amount > 0') }

  private

  def debit_or_credit_not_both
    return if debit_amount.zero? || credit_amount.zero?

    errors.add(:base, '借方金額と貸方金額の両方に値を設定することはできません')
  end
end
