# frozen_string_literal: true

# 自動仕訳管理モデル
# ソースデータの最終処理日時を管理する（日付管理方式）
class AutoJournalManagement < ApplicationRecord
  validates :source_table_name, presence: true, uniqueness: true

  # 最終処理日時以降のレコードを取得
  def self.fetch_unprocessed_records(source_table_name, model_class)
    management = find_or_create_by(source_table_name: source_table_name)

    if management.last_processed_at.present?
      model_class.where('updated_at > ?', management.last_processed_at)
    else
      model_class.all
    end
  end

  # 最終処理日時を更新
  def update_last_processed_at!(timestamp = Time.current)
    update!(last_processed_at: timestamp)
  end
end
