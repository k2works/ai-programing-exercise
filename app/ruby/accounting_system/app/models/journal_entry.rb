# frozen_string_literal: true

# 仕訳エントリモデル
# 複式簿記の仕訳データを管理する
class JournalEntry < ApplicationRecord
  # アソシエーション
  has_many :details, class_name: 'JournalEntryDetail', dependent: :destroy

  # バリデーション
  validates :entry_number, presence: true, uniqueness: true, length: { maximum: 10 }
  validates :entry_date, presence: true
  validates :description, presence: true, length: { maximum: 100 }
  validates :total_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
  validates :created_by, presence: true

  # スコープ
  scope :by_date, ->(date) { where(entry_date: date) }
  scope :by_date_range, ->(start_date, end_date) { where(entry_date: start_date..end_date) }
  scope :ordered, -> { order(entry_date: :desc, entry_number: :desc) }

  # 借方合計
  def debit_total
    details.sum(:debit_amount)
  end

  # 貸方合計
  def credit_total
    details.sum(:credit_amount)
  end

  # 貸借平衡チェック
  def balanced?
    debit_total == credit_total
  end
end
