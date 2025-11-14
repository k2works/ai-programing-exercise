# frozen_string_literal: true

class Employee < ApplicationRecord
  # 関連
  belongs_to :department

  # バリデーション
  validates :code, presence: true, uniqueness: true
  validates :first_name, presence: true
  validates :last_name, presence: true

  # メソッド
  def full_name
    "#{last_name} #{first_name}"
  end
end
