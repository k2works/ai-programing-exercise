# frozen_string_literal: true

# 自動仕訳パターン明細モデル
# パターンの明細行を管理する
class AutoJournalPatternItem < ApplicationRecord
  belongs_to :auto_journal_pattern

  validates :line_number, presence: true, uniqueness: { scope: :auto_journal_pattern_id }
  validates :debit_credit_flag, presence: true, inclusion: { in: %w[D C] }
  validates :account_code, presence: true
  validates :amount_expression, presence: true

  default_scope -> { order(:line_number) }
end
