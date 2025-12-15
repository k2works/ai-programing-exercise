class CreateAuditLogs < ActiveRecord::Migration[8.0]
  def change
    create_table :audit_logs do |t|
      t.string :entity_type, null: false, limit: 50
      t.string :entity_id, null: false, limit: 100
      t.string :action, null: false, limit: 20
      t.string :user_id, null: false, limit: 100
      t.string :user_name, null: false, limit: 200
      t.datetime :timestamp, null: false
      t.jsonb :old_values
      t.jsonb :new_values
      t.jsonb :changes
      t.text :reason
      t.string :ip_address, limit: 45
      t.text :user_agent

      t.timestamps
    end

    # インデックス作成
    add_index :audit_logs, [:entity_type, :entity_id], name: 'idx_audit_log_entity'
    add_index :audit_logs, :user_id, name: 'idx_audit_log_user'
    add_index :audit_logs, :timestamp, name: 'idx_audit_log_timestamp'
    add_index :audit_logs, :action, name: 'idx_audit_log_action'
    
    # JSONB 列にもインデックス
    add_index :audit_logs, :changes, using: :gin, name: 'idx_audit_log_changes'

    # コメント
    change_table_comment :audit_logs, '監査ログテーブル（Append-Onlyで不変）'
    change_column_comment :audit_logs, :entity_type, 'エンティティ種別（Journal, Account等）'
    change_column_comment :audit_logs, :entity_id, 'エンティティID'
    change_column_comment :audit_logs, :action, '操作種別（create, update, delete）'
    change_column_comment :audit_logs, :old_values, '変更前の値（UPDATE, DELETE時）'
    change_column_comment :audit_logs, :new_values, '変更後の値（UPDATE時）'
    change_column_comment :audit_logs, :changes, '変更内容（CREATE時）'
    change_column_comment :audit_logs, :reason, '操作理由（任意）'
  end
end
