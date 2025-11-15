# frozen_string_literal: true

class ProductCategory < ApplicationRecord
  # バリデーション
  validates :code, presence: true, uniqueness: true
  validates :name, presence: true

  # 関連
  has_many :products, dependent: :destroy
end
