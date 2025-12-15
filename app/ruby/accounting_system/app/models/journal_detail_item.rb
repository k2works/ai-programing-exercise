# frozen_string_literal: true

# 仕訳貸借明細モデル（3層構造の最下層）
# 仕訳の借方・貸方の詳細データを管理する
class JournalDetailItem < ApplicationRecord
  # 関連
  belongs_to :journal_detail
  has_one :journal, through: :journal_detail
  belongs_to :account, foreign_key: :account_code, primary_key: :code, optional: true

  # バリデーション
  validates :debit_credit_type, presence: true, inclusion: { in: %w[D C] }
  validates :currency_code, presence: true, length: { is: 3 }
  validates :exchange_rate, presence: true, numericality: { greater_than: 0 }
  validates :account_code, presence: true
  validates :amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
  validates :base_amount, presence: true, numericality: { greater_than_or_equal_to: 0 }
  validates :cash_flow_flag, inclusion: { in: [0, 1] }

  # スコープ
  scope :debit, -> { where(debit_credit_type: 'D') }
  scope :credit, -> { where(debit_credit_type: 'C') }

  # デフォルトスコープ（貸借区分順）
  default_scope -> { order(:debit_credit_type) }

  # 借方か貸方かを判定
  def debit?
    debit_credit_type == 'D'
  end

  def credit?
    debit_credit_type == 'C'
  end
end
