# frozen_string_literal: true

class CreateJournalEntries < ActiveRecord::Migration[8.1]
  def change
    # 仕訳エントリテーブル
    create_table :journal_entries, id: false, comment: '仕訳エントリ' do |t|
      t.string :voucher_no, primary_key: true, null: false, limit: 20, comment: '伝票番号'
      t.date :journal_date, null: false, comment: '仕訳日'
      t.string :description, null: false, limit: 200, comment: '摘要'
      t.decimal :total_amount, precision: 15, scale: 0, null: false, default: 0, comment: '合計金額'
      t.integer :fiscal_year, null: false, comment: '会計年度'

      t.timestamps
    end

    add_index :journal_entries, :journal_date
    add_index :journal_entries, :fiscal_year

    # 仕訳明細テーブル
    create_table :journal_entry_details, id: false, comment: '仕訳明細' do |t|
      t.string :voucher_no, null: false, limit: 20, comment: '伝票番号'
      t.integer :line_number, null: false, comment: '行番号'
      t.string :account_code, null: false, limit: 10, comment: '勘定科目コード'
      t.decimal :debit_amount, precision: 15, scale: 0, null: false, default: 0, comment: '借方金額'
      t.decimal :credit_amount, precision: 15, scale: 0, null: false, default: 0, comment: '貸方金額'
      t.string :description, limit: 200, comment: '摘要'

      t.timestamps
    end

    add_index :journal_entry_details, [:voucher_no, :line_number], unique: true
    add_foreign_key :journal_entry_details, :journal_entries, column: :voucher_no, primary_key: :voucher_no, on_delete: :cascade
  end
end
