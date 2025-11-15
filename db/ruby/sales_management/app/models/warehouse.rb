# frozen_string_literal: true

class Warehouse < ApplicationRecord
  TYPES = { own: 1, consigned: 2 }.freeze

  validates :code, presence: true, uniqueness: true
  validates :name, presence: true
  validates :warehouse_type, inclusion: { in: TYPES.values }

  has_many :stocks, dependent: :destroy
  has_many :purchase_orders, dependent: :restrict_with_error
end
