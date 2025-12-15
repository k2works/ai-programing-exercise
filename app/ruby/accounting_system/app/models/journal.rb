# frozen_string_literal: true

# 仕訳モデル（3層構造のヘッダー）
# 複式簿記の仕訳データを管理する
class Journal < ApplicationRecord
  include DoubleEntryValidatable

  # 関連
  has_many :details, class_name: 'JournalDetail', dependent: :destroy
  has_many :detail_items, through: :details, source: :items

  # バリデーション
  validates :journal_no, presence: true, uniqueness: true
  validates :journal_date, presence: true
  validates :input_date, presence: true
  validates :settlement_flag, inclusion: { in: [0, 1] }
  validates :single_entry_flag, inclusion: { in: [0, 1] }
  validates :red_slip_flag, inclusion: { in: [0, 1] }

  # カスタムバリデーション
  validate :red_slip_must_have_voucher_number

  # スコープ
  scope :normal, -> { where(settlement_flag: 0) }
  scope :settlement, -> { where(settlement_flag: 1) }
  scope :single_entry, -> { where(single_entry_flag: 1) }
  scope :compound_entry, -> { where(single_entry_flag: 0) }
  scope :red_slip, -> { where(red_slip_flag: 1) }

  # 借方・貸方の合計を取得
  def debit_total
    detail_items.where(debit_credit_type: 'D').sum(:amount)
  end

  def credit_total
    detail_items.where(debit_credit_type: 'C').sum(:amount)
  end

  # 貸借平衡チェック
  def balanced?
    debit_total == credit_total
  end

  private

  def red_slip_must_have_voucher_number
    return unless red_slip_flag == 1 && red_black_slip_number.blank?

    errors.add(:red_black_slip_number, '赤伝票の場合は赤黒伝票番号が必須です')
  end
end
