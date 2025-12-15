# frozen_string_literal: true

class CreateJournals < ActiveRecord::Migration[8.1]
  def change
    # 1. 仕訳テーブル（ヘッダー）
    create_table :journals do |t|
      t.string :journal_no, limit: 20, null: false
      t.date :journal_date, null: false
      t.date :input_date, null: false
      t.integer :settlement_flag, default: 0, null: false
      t.integer :single_entry_flag, default: 0, null: false
      t.integer :slip_type, default: 0, null: false
      t.integer :periodic_flag, default: 0, null: false
      t.string :employee_code, limit: 10
      t.string :department_code, limit: 5
      t.integer :red_slip_flag, default: 0, null: false
      t.string :red_black_slip_number, limit: 20

      t.timestamps
    end

    add_index :journals, :journal_no, unique: true
    add_index :journals, :journal_date
    add_index :journals, :department_code
    add_index :journals, :red_slip_flag

    # CHECK制約
    execute <<-SQL.squish
      ALTER TABLE journals
        ADD CONSTRAINT check_settlement_flag
        CHECK (settlement_flag IN (0, 1))
    SQL

    execute <<-SQL.squish
      ALTER TABLE journals
        ADD CONSTRAINT check_red_slip_flag
        CHECK (red_slip_flag IN (0, 1))
    SQL

    # 赤伝票の場合は赤黒伝票番号が必須
    execute <<-SQL.squish
      ALTER TABLE journals
        ADD CONSTRAINT check_red_slip_voucher
        CHECK (
          (red_slip_flag = 0)
          OR
          (red_slip_flag = 1 AND red_black_slip_number IS NOT NULL)
        )
    SQL

    # 2. 仕訳明細テーブル
    create_table :journal_details do |t|
      t.references :journal, null: false, foreign_key: { on_delete: :cascade }
      t.integer :line_number, null: false
      t.string :description, limit: 1000, null: false

      t.timestamps
    end

    add_index :journal_details, %i[journal_id line_number], unique: true

    # 3. 仕訳貸借明細テーブル
    create_table :journal_detail_items do |t|
      t.references :journal_detail, null: false, foreign_key: { on_delete: :cascade }
      t.string :debit_credit_type, limit: 1, null: false
      t.string :currency_code, limit: 3, default: 'JPY', null: false
      t.decimal :exchange_rate, precision: 10, scale: 4, default: 1.0000, null: false
      t.string :department_code, limit: 5
      t.string :project_code, limit: 10
      t.string :account_code, limit: 10, null: false
      t.string :sub_account_code, limit: 10
      t.decimal :amount, precision: 15, scale: 2, null: false
      t.decimal :base_amount, precision: 15, scale: 2, null: false
      t.string :tax_type, limit: 2
      t.integer :tax_rate
      t.string :tax_calc_type, limit: 2
      t.date :due_date
      t.integer :cash_flow_flag, default: 0, null: false
      t.string :segment_code, limit: 10
      t.string :offset_account_code, limit: 10
      t.string :offset_sub_account_code, limit: 10
      t.string :note_code, limit: 1
      t.string :note_content, limit: 60

      t.timestamps
    end

    add_index :journal_detail_items, :department_code
    add_index :journal_detail_items, :project_code
    add_index :journal_detail_items, :account_code
    add_index :journal_detail_items, %i[journal_detail_id debit_credit_type],
              unique: true, name: 'idx_journal_detail_items_unique'

    # CHECK制約
    execute <<-SQL.squish
      ALTER TABLE journal_detail_items
        ADD CONSTRAINT check_debit_credit_type
        CHECK (debit_credit_type IN ('D', 'C'))
    SQL

    execute <<-SQL.squish
      ALTER TABLE journal_detail_items
        ADD CONSTRAINT check_amount
        CHECK (amount >= 0)
    SQL

    execute <<-SQL.squish
      ALTER TABLE journal_detail_items
        ADD CONSTRAINT check_exchange_rate
        CHECK (exchange_rate > 0)
    SQL

    execute <<-SQL.squish
      ALTER TABLE journal_detail_items
        ADD CONSTRAINT check_currency_code_length
        CHECK (LENGTH(currency_code) = 3)
    SQL
  end
end
