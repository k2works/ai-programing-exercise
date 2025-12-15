# frozen_string_literal: true

class CreateJournalEntryDetails < ActiveRecord::Migration[8.1]
  def change
    create_table :journal_entry_details do |t|
      t.references :journal_entry, null: false, foreign_key: { on_delete: :cascade }
      t.integer :line_number, null: false
      t.string :account_code, limit: 20, null: false
      t.decimal :debit_amount, precision: 15, scale: 2, default: 0, null: false
      t.decimal :credit_amount, precision: 15, scale: 2, default: 0, null: false
      t.string :description, limit: 100, null: false
      t.decimal :tax_amount, precision: 15, scale: 2, default: 0
      t.decimal :tax_rate, precision: 5, scale: 2

      t.timestamps
    end

    add_index :journal_entry_details, %i[journal_entry_id line_number], unique: true
    add_index :journal_entry_details, :account_code

    add_foreign_key :journal_entry_details, :accounts,
                    column: :account_code,
                    primary_key: :code

    # テーブルコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON TABLE journal_entry_details IS '仕訳明細（仕訳エントリの明細行データ）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.journal_entry_id IS '仕訳エントリID（外部キー）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.line_number IS '行番号';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.account_code IS '勘定科目コード';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.debit_amount IS '借方金額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.credit_amount IS '貸方金額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.description IS '摘要';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.tax_amount IS '消費税額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN journal_entry_details.tax_rate IS '消費税率';
        SQL
      end
    end
  end
end
