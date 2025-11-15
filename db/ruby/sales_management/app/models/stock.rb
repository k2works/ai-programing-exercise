# frozen_string_literal: true

class Stock < ApplicationRecord
  self.primary_key = %i[warehouse_id product_id lot_number stock_type quality_type]

  STOCK_TYPES = %w[normal consignment].freeze
  QUALITY_TYPES = %w[good defective hold].freeze

  belongs_to :warehouse
  belongs_to :product

  validates :lot_number, presence: true
  validates :stock_type, presence: true, inclusion: { in: STOCK_TYPES }
  validates :quality_type, presence: true, inclusion: { in: QUALITY_TYPES }
  validates :actual_quantity, numericality: { greater_than_or_equal_to: 0 }
  validates :valid_quantity, numericality: { greater_than_or_equal_to: 0 }

  # 引当数量（実在庫 - 有効在庫）
  def allocated_quantity
    actual_quantity - valid_quantity
  end

  # 在庫充足チェック
  def sufficient?(required_quantity)
    valid_quantity >= required_quantity
  end

  # 在庫引当
  def allocate(quantity)
    raise ArgumentError, '引当数量が有効在庫を超えています' unless sufficient?(quantity)

    self.valid_quantity -= quantity
    save!
  end

  # 在庫引当解除
  def deallocate(quantity)
    self.valid_quantity += quantity
    self.valid_quantity = [valid_quantity, actual_quantity].min # 実在庫を超えないように
    save!
  end
end
