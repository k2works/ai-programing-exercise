# frozen_string_literal: true

# 自動仕訳実行ログモデル
# 自動仕訳の実行履歴を管理する
class AutoJournalLog < ApplicationRecord
  belongs_to :auto_journal_pattern

  validates :executed_at, presence: true
  validates :status, presence: true, inclusion: { in: %w[success failed partial] }

  scope :successful, -> { where(status: 'success') }
  scope :failed, -> { where(status: 'failed') }
  scope :recent, -> { order(executed_at: :desc) }

  # 実行ログを記録
  def self.record_execution(pattern, processed_count, generated_count, status, message = nil, error_detail = nil)
    create!(
      auto_journal_pattern: pattern,
      executed_at: Time.current,
      processed_count: processed_count,
      generated_count: generated_count,
      status: status,
      message: message,
      error_detail: error_detail
    )
  end
end
