# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# This file is the source Rails uses to define your schema when running `bin/rails
# db:schema:load`. When creating a new database, `bin/rails db:schema:load` tends to
# be faster and is potentially less error prone than running all of your
# migrations from scratch. Old migrations may fail to apply correctly if those
# migrations use external dependencies or application code.
#
# It's strongly recommended that you check this file into your version control system.

ActiveRecord::Schema[8.1].define(version: 2025_12_16_025326) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "pg_catalog.plpgsql"

  create_table "journal_entries", primary_key: "voucher_no", id: { type: :string, limit: 20, comment: "伝票番号" }, comment: "仕訳エントリ", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.string "description", limit: 200, null: false, comment: "摘要"
    t.integer "fiscal_year", null: false, comment: "会計年度"
    t.date "journal_date", null: false, comment: "仕訳日"
    t.decimal "total_amount", precision: 15, default: "0", null: false, comment: "合計金額"
    t.datetime "updated_at", null: false
    t.index ["fiscal_year"], name: "index_journal_entries_on_fiscal_year"
    t.index ["journal_date"], name: "index_journal_entries_on_journal_date"
  end

  create_table "journal_entry_details", id: false, comment: "仕訳明細", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false, comment: "勘定科目コード"
    t.datetime "created_at", null: false
    t.decimal "credit_amount", precision: 15, default: "0", null: false, comment: "貸方金額"
    t.decimal "debit_amount", precision: 15, default: "0", null: false, comment: "借方金額"
    t.string "description", limit: 200, comment: "摘要"
    t.integer "line_number", null: false, comment: "行番号"
    t.datetime "updated_at", null: false
    t.string "voucher_no", limit: 20, null: false, comment: "伝票番号"
    t.index ["voucher_no", "line_number"], name: "index_journal_entry_details_on_voucher_no_and_line_number", unique: true
  end

  add_foreign_key "journal_entry_details", "journal_entries", column: "voucher_no", primary_key: "voucher_no", on_delete: :cascade
end
