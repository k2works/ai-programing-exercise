# frozen_string_literal: true

class CreateAutoJournalTables < ActiveRecord::Migration[8.1]
  def change
    # 1. 自動仕訳管理テーブル
    create_table :auto_journal_managements do |t|
      t.string :source_table_name, limit: 100, null: false
      t.timestamp :last_processed_at

      t.timestamps
    end

    add_index :auto_journal_managements, :source_table_name, unique: true

    # 2. 自動仕訳パターンテーブル
    create_table :auto_journal_patterns do |t|
      t.string :pattern_code, limit: 20, null: false
      t.string :pattern_name, limit: 100, null: false
      t.string :source_table_name, limit: 100, null: false
      t.string :description, limit: 500
      t.boolean :is_active, default: true, null: false

      t.timestamps
    end

    add_index :auto_journal_patterns, :pattern_code, unique: true
    add_index :auto_journal_patterns, :is_active

    # 3. 自動仕訳パターン明細テーブル
    create_table :auto_journal_pattern_items do |t|
      t.references :auto_journal_pattern, null: false, foreign_key: { on_delete: :cascade }
      t.integer :line_number, null: false
      t.string :debit_credit_flag, limit: 1, null: false
      t.string :account_code, limit: 10, null: false
      t.string :amount_expression, limit: 200, null: false
      t.string :description_template, limit: 200

      t.timestamps
    end

    add_index :auto_journal_pattern_items, %i[auto_journal_pattern_id line_number],
              unique: true, name: 'idx_auto_journal_pattern_items_unique'

    # CHECK制約
    execute <<-SQL.squish
      ALTER TABLE auto_journal_pattern_items
        ADD CONSTRAINT check_debit_credit_flag
        CHECK (debit_credit_flag IN ('D', 'C'))
    SQL

    # 4. 自動仕訳実行ログテーブル
    create_table :auto_journal_logs do |t|
      t.references :auto_journal_pattern, null: false, foreign_key: { on_delete: :cascade }
      t.timestamp :executed_at, null: false
      t.integer :processed_count, default: 0, null: false
      t.integer :generated_count, default: 0, null: false
      t.string :status, limit: 20, null: false
      t.string :message, limit: 500
      t.text :error_detail

      t.timestamps
    end

    add_index :auto_journal_logs, :executed_at
    add_index :auto_journal_logs, :status
  end
end
