class RenameChangesToChangeDataInAuditLogs < ActiveRecord::Migration[8.0]
  def change
    rename_column :audit_logs, :changes, :change_data
    change_column_comment :audit_logs, :change_data, '変更内容（CREATE時）'
  end
end
