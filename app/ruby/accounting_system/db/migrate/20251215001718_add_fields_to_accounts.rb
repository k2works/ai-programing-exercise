# frozen_string_literal: true

class AddFieldsToAccounts < ActiveRecord::Migration[8.1]
  def change
    change_table :accounts, bulk: true do |t|
      # BSPL区分を追加
      t.string :bspl_type, limit: 1

      # 貸借区分を追加
      t.string :debit_credit_type, limit: 1

      # 集計科目フラグを追加
      t.boolean :is_summary, default: false, null: false
    end

    # カラムコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.bspl_type IS 'BSPL区分（B:貸借対照表, P:損益計算書）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.debit_credit_type IS '貸借区分（D:借方, C:貸方）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.is_summary IS '集計科目フラグ（true:集計科目, false:明細科目）';
        SQL
      end
    end
  end
end
