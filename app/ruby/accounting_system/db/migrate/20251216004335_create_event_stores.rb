# frozen_string_literal: true

class CreateEventStores < ActiveRecord::Migration[8.0]
  def change
    create_table :event_stores do |t|
      # Aggregate 識別子
      t.string :aggregate_id, null: false, limit: 100
      t.string :aggregate_type, null: false, limit: 50

      # イベントメタデータ
      t.string :event_type, null: false, limit: 100
      t.integer :event_version, null: false, default: 1

      # イベントデータ（JSONB形式）
      t.jsonb :event_data, null: false

      # メタデータ
      t.datetime :occurred_at, null: false, default: -> { 'CURRENT_TIMESTAMP' }
      t.string :user_id, limit: 100
      t.string :correlation_id, limit: 100
      t.string :causation_id, limit: 100

      # 楽観的ロック（同時実行制御）
      t.integer :sequence_number, null: false

      t.timestamps
    end

    # 一意性制約（同一 Aggregate の sequence_number は一意）
    add_index :event_stores, [:aggregate_id, :sequence_number],
              unique: true, name: 'uk_aggregate_sequence'

    # インデックス設計
    add_index :event_stores, [:aggregate_id, :sequence_number],
              name: 'idx_event_store_aggregate_id'
    add_index :event_stores, :event_type,
              name: 'idx_event_store_event_type'
    add_index :event_stores, :occurred_at,
              name: 'idx_event_store_occurred_at'
    add_index :event_stores, :correlation_id,
              name: 'idx_event_store_correlation_id'

    # JSONB用のGINインデックス
    add_index :event_stores, :event_data, using: :gin,
              name: 'idx_event_store_event_data'

    # カラムコメント
    change_table_comment :event_stores, 'イベントストア（Event Sourcing用、Append-Onlyで不変）'
    change_column_comment :event_stores, :aggregate_id, 'Aggregateのユニーク識別子（例: "account-1001"）'
    change_column_comment :event_stores, :aggregate_type, 'Aggregateの型（例: "Account", "JournalEntry"）'
    change_column_comment :event_stores, :event_type, 'イベントの型（例: "AccountCreatedEvent"）'
    change_column_comment :event_stores, :event_version, 'イベント定義のバージョン（スキーマ進化に対応）'
    change_column_comment :event_stores, :event_data, 'イベントのペイロード（JSONB形式）'
    change_column_comment :event_stores, :occurred_at, 'イベントが発生した日時'
    change_column_comment :event_stores, :user_id, 'イベントを発生させたユーザーID'
    change_column_comment :event_stores, :correlation_id, '関連する一連のイベントをグループ化'
    change_column_comment :event_stores, :causation_id, '因果関係のあるイベントID'
    change_column_comment :event_stores, :sequence_number, 'Aggregate内でのイベントの順序番号'
  end
end
