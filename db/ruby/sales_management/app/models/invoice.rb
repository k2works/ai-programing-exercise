# frozen_string_literal: true

class Invoice < ApplicationRecord
  belongs_to :party
  has_many :invoice_items, dependent: :destroy
  has_many :orders, through: :invoice_items
  has_many :payments, dependent: :restrict_with_error

  validates :invoice_number, presence: true, uniqueness: true
  validates :invoice_date, presence: true
  validates :closing_date, presence: true
  validates :due_date, presence: true

  def total_amount
    invoice_items.sum(:amount)
  end

  def paid_amount
    payments.sum(:amount)
  end

  def balance
    total_amount - paid_amount
  end

  def fully_paid?
    balance <= 0
  end
end
