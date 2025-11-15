# frozen_string_literal: true

class Purchase < ApplicationRecord
  belongs_to :purchase_order
  belongs_to :party
  has_many :purchase_items, dependent: :destroy
  has_many :products, through: :purchase_items

  validates :purchase_number, presence: true, uniqueness: true
  validates :purchase_date, presence: true

  after_create :update_stock

  def calculate_total
    purchase_items.sum(&:subtotal)
  end

  private

  def update_stock
    purchase_items.each do |item|
      stock = Stock.find_or_initialize_by(
        warehouse_id: item.warehouse_id,
        product_id: item.product_id,
        lot_number: item.lot_number,
        stock_type: 'normal',
        quality_type: 'good'
      )

      stock.actual_quantity += item.quantity
      stock.valid_quantity += item.quantity
      stock.save!

      # 発注明細の入荷数量を更新
      po_item = purchase_order.purchase_order_items.find_by(product_id: item.product_id)
      po_item&.increment!(:received_quantity, item.quantity)
    end
  end
end
