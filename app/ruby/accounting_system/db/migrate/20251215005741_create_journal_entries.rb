# frozen_string_literal: true

class CreateJournalEntries < ActiveRecord::Migration[8.1]
  def change
    create_table :journal_entries do |t|
      t.string :entry_number, limit: 10, null: false
      t.date :entry_date, null: false
      t.string :description, limit: 100, null: false
      t.decimal :total_amount, precision: 15, scale: 2, null: false
      t.string :reference_number, limit: 20
      t.string :created_by, limit: 20, null: false
      t.string :updated_by, limit: 20

      t.timestamps
    end

    add_index :journal_entries, :entry_number, unique: true
    add_index :journal_entries, :entry_date

    # テーブルコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON TABLE journal_entries IS '仕訳エントリ（複式簿記の仕訳データ）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.entry_number IS '伝票番号（主キー）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.entry_date IS '仕訳日';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.description IS '摘要';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.total_amount IS '合計金額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.reference_number IS '参照番号';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.created_by IS '作成者';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entries.updated_by IS '更新者';
        SQL
      end
    end
  end
end
