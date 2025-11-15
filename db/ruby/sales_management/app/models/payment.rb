# frozen_string_literal: true

class Payment < ApplicationRecord
  METHODS = %w[cash bank_transfer credit_card].freeze

  belongs_to :invoice

  validates :payment_date, presence: true
  validates :amount, presence: true, numericality: { greater_than: 0 }
  validates :payment_method, inclusion: { in: METHODS }, allow_nil: true
  validate :amount_not_exceeds_balance

  after_create :check_full_payment

  private

  def amount_not_exceeds_balance
    return unless invoice && amount

    remaining = invoice.balance
    return unless amount > remaining

    errors.add(:amount, 'が請求残高を超えています')
  end

  def check_full_payment
    return unless invoice.fully_paid?

    Rails.logger.info "請求書 #{invoice.invoice_number} が完済されました"
  end
end
