# frozen_string_literal: true

class PurchaseItem < ApplicationRecord
  belongs_to :purchase
  belongs_to :product
  belongs_to :warehouse

  validates :lot_number, presence: true
  validates :quantity, presence: true, numericality: { greater_than: 0 }
  validates :unit_price, presence: true, numericality: { greater_than_or_equal_to: 0 }

  def subtotal
    quantity * unit_price
  end
end
