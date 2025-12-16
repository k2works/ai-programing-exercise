# frozen_string_literal: true

class EventStore < ApplicationRecord
  # 不変性の保証（読み取り専用）
  def readonly?
    persisted?
  end

  # バリデーション
  validates :aggregate_id, presence: true, length: { maximum: 100 }
  validates :aggregate_type, presence: true, length: { maximum: 50 }
  validates :event_type, presence: true, length: { maximum: 100 }
  validates :event_version, presence: true, numericality: { only_integer: true, greater_than: 0 }
  validates :event_data, presence: true
  validates :sequence_number, presence: true, numericality: { only_integer: true, greater_than: 0 }
  validates :occurred_at, presence: true

  # スコープ
  scope :for_aggregate, lambda { |aggregate_id|
    where(aggregate_id: aggregate_id).order(:sequence_number)
  }

  scope :by_type, lambda { |event_type|
    where(event_type: event_type).order(:occurred_at)
  }

  scope :by_correlation, lambda { |correlation_id|
    where(correlation_id: correlation_id).order(:occurred_at)
  }

  scope :since, lambda { |timestamp|
    where('occurred_at >= ?', timestamp).order(:occurred_at)
  }

  scope :until, lambda { |timestamp|
    where('occurred_at <= ?', timestamp).order(:occurred_at)
  }

  # Aggregate の最新シーケンス番号を取得
  def self.latest_sequence_for(aggregate_id)
    where(aggregate_id: aggregate_id).maximum(:sequence_number) || 0
  end

  # Aggregate のすべてのイベントを取得
  def self.events_for(aggregate_id)
    for_aggregate(aggregate_id).to_a
  end

  # 特定時点までのイベントを取得
  def self.events_until(aggregate_id, timestamp)
    for_aggregate(aggregate_id).until(timestamp).to_a
  end

  # 同時実行制御（楽観的ロック）
  class ConcurrentModificationError < StandardError; end

  def self.save_event(aggregate_id:, aggregate_type:, event_type:, event_version:, event_data:,
                      sequence_number:, user_id: nil, correlation_id: nil, causation_id: nil, occurred_at: Time.current)
    create!(
      aggregate_id: aggregate_id,
      aggregate_type: aggregate_type,
      event_type: event_type,
      event_version: event_version,
      event_data: event_data,
      sequence_number: sequence_number,
      occurred_at: occurred_at,
      user_id: user_id,
      correlation_id: correlation_id,
      causation_id: causation_id
    )
  rescue ActiveRecord::RecordNotUnique => e
    raise ConcurrentModificationError,
          "Expected sequence #{sequence_number} but conflict occurred: #{e.message}"
  end
end
