# frozen_string_literal: true

class Product < ApplicationRecord
  # 関連
  belongs_to :product_category

  # バリデーション
  validates :code, presence: true, uniqueness: true
  validates :name, presence: true
  validates :unit_price, presence: true, numericality: { greater_than_or_equal_to: 0 }
end
