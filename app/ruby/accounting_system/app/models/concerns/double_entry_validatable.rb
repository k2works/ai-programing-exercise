# frozen_string_literal: true

# 複式簿記の原理（借方合計 = 貸方合計）を検証するコンサーン
module DoubleEntryValidatable
  extend ActiveSupport::Concern

  included do
    validate :debit_credit_must_balance, if: :persisted?
  end

  private

  def debit_credit_must_balance
    return unless detail_items.any?

    debit_total = detail_items.debit.sum(:amount)
    credit_total = detail_items.credit.sum(:amount)

    return if debit_total == credit_total

    errors.add(:base, "借方合計（#{debit_total}）と貸方合計（#{credit_total}）が一致しません")
  end
end
