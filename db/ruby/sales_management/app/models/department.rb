# frozen_string_literal: true

class Department < ApplicationRecord
  # バリデーション
  validates :code, presence: true, uniqueness: true
  validates :name, presence: true

  # 関連
  has_many :employees, dependent: :destroy
end
