# frozen_string_literal: true

class AuditLog < ApplicationRecord
  # 不変性の保証（読み取り専用）
  def readonly?
    persisted?
  end

  # アクションの列挙型
  enum :action, {
    create: 'create',
    update: 'update',
    delete: 'delete'
  }, suffix: true

  # バリデーション
  validates :entity_type, presence: true, length: { maximum: 50 }
  validates :entity_id, presence: true, length: { maximum: 100 }
  validates :action, presence: true
  validates :user_id, presence: true, length: { maximum: 100 }
  validates :user_name, presence: true, length: { maximum: 200 }
  validates :timestamp, presence: true

  # スコープ
  scope :by_entity, lambda { |entity_type, entity_id|
    where(entity_type: entity_type, entity_id: entity_id).order(timestamp: :desc)
  }

  scope :by_user, lambda { |user_id|
    where(user_id: user_id).order(timestamp: :desc)
  }

  scope :by_period, lambda { |start_date, end_date|
    where(timestamp: start_date..end_date).order(timestamp: :desc)
  }

  scope :by_action, lambda { |action|
    where(action: action).order(timestamp: :desc)
  }

  # ファクトリメソッド：CREATE操作用
  def self.create_log(entity_type:, entity_id:, action:, user_id:, user_name:, change_data:, ip_address:, user_agent: nil)
    create!(
      entity_type: entity_type,
      entity_id: entity_id,
      action: action,
      user_id: user_id,
      user_name: user_name,
      timestamp: Time.current,
      change_data: change_data,
      ip_address: ip_address,
      user_agent: user_agent
    )
  end

  # ファクトリメソッド：UPDATE操作用
  def self.create_for_update(entity_type:, entity_id:, user_id:, user_name:, old_values:, new_values:, ip_address:,
                              user_agent: nil)
    create!(
      entity_type: entity_type,
      entity_id: entity_id,
      action: :update,
      user_id: user_id,
      user_name: user_name,
      timestamp: Time.current,
      old_values: old_values,
      new_values: new_values,
      ip_address: ip_address,
      user_agent: user_agent
    )
  end

  # ファクトリメソッド：DELETE操作用
  def self.create_for_delete(entity_type:, entity_id:, user_id:, user_name:, old_values:, reason:, ip_address:,
                              user_agent: nil)
    create!(
      entity_type: entity_type,
      entity_id: entity_id,
      action: :delete,
      user_id: user_id,
      user_name: user_name,
      timestamp: Time.current,
      old_values: old_values,
      reason: reason,
      ip_address: ip_address,
      user_agent: user_agent
    )
  end

  # サマリー文字列を生成
  def summary
    "#{entity_type} #{entity_id} を#{action_display_name}"
  end

  private

  def action_display_name
    case action
    when 'create' then '作成'
    when 'update' then '更新'
    when 'delete' then '削除'
    else action
    end
  end
end
