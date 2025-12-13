# frozen_string_literal: true

class Account < ApplicationRecord
  # 勘定科目種別のenum定義
  enum :account_type, {
    asset: 0,       # 資産
    liability: 1,   # 負債
    equity: 2,      # 純資産
    revenue: 3,     # 収益
    expense: 4      # 費用
  }

  # バリデーション
  validates :code, presence: true, uniqueness: true
  validates :name, presence: true
  validates :account_type, presence: true
end
