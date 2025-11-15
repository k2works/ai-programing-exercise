# frozen_string_literal: true

class Bill < ApplicationRecord
  belongs_to :party
  has_many :bill_items, dependent: :destroy
  has_many :purchases, through: :bill_items
  has_many :bill_payments, dependent: :restrict_with_error

  validates :bill_number, presence: true, uniqueness: true
  validates :bill_date, presence: true
  validates :closing_date, presence: true
  validates :due_date, presence: true

  def total_amount
    bill_items.sum(:amount)
  end

  def paid_amount
    bill_payments.sum(:amount)
  end

  def balance
    total_amount - paid_amount
  end

  def fully_paid?
    balance <= 0
  end
end
