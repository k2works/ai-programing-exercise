# frozen_string_literal: true

class BillPayment < ApplicationRecord
  METHODS = %w[cash bank_transfer credit_card].freeze

  belongs_to :bill

  validates :payment_date, presence: true
  validates :amount, presence: true, numericality: { greater_than: 0 }
  validates :payment_method, inclusion: { in: METHODS }, allow_nil: true
  validate :amount_not_exceeds_balance

  private

  def amount_not_exceeds_balance
    return unless bill && amount

    remaining = bill.balance
    return unless amount > remaining

    errors.add(:amount, 'が支払残高を超えています')
  end
end
