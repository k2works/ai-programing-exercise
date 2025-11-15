# frozen_string_literal: true

class PurchaseOrderItem < ApplicationRecord
  belongs_to :purchase_order
  belongs_to :product

  validates :quantity, presence: true, numericality: { greater_than: 0 }
  validates :unit_price, presence: true, numericality: { greater_than_or_equal_to: 0 }

  before_validation :set_defaults, on: :create

  def subtotal
    quantity * unit_price
  end

  def remaining_quantity
    quantity - received_quantity
  end

  private

  def set_defaults
    self.received_quantity ||= 0
  end
end
