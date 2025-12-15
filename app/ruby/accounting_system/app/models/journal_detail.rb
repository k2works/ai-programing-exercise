# frozen_string_literal: true

# 仕訳明細モデル（3層構造の中間層）
# 仕訳の明細行を管理する
class JournalDetail < ApplicationRecord
  # 関連
  belongs_to :journal
  has_many :items, class_name: 'JournalDetailItem', dependent: :destroy

  # バリデーション
  validates :line_number, presence: true
  validates :description, presence: true, length: { maximum: 1000 }
  validates :line_number, uniqueness: { scope: :journal_id }

  # デフォルトスコープ（行番号順）
  default_scope -> { order(:line_number) }
end
