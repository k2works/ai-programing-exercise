# frozen_string_literal: true

class CreditLimit < ApplicationRecord
  belongs_to :party

  validates :limit_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }

  before_validation :set_defaults, on: :create

  # 与信チェック（注文金額のみ）
  def available?(amount)
    amount <= limit_amount
  end

  # 与信チェック（未払残高を含む）
  def available_with_outstanding?(amount)
    outstanding = calculate_outstanding
    (outstanding + amount) <= limit_amount
  end

  # 未払残高を計算
  def calculate_outstanding
    party.invoices.map(&:balance).sum
  end

  # 利用可能額
  def available_amount
    limit_amount - calculate_outstanding
  end

  private

  def set_defaults
    self.used_amount ||= 0
  end
end
