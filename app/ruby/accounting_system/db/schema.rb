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

ActiveRecord::Schema[8.1].define(version: 2025_12_16_004335) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "pg_catalog.plpgsql"

  create_table "account_structures", primary_key: "account_code", id: { type: :string, limit: 20, comment: "勘定科目コード" }, comment: "勘定科目の階層構造を管理するマスタテーブル", force: :cascade do |t|
    t.string "account_path", limit: 200, null: false, comment: "チルダ連結形式のパス（例: 11~11000~11190~11110）"
    t.datetime "created_at", null: false
    t.integer "display_order", default: 0, null: false, comment: "同じ階層内での表示順序"
    t.integer "hierarchy_level", default: 1, null: false, comment: "階層の深さ（ルート=1）"
    t.string "parent_code", limit: 20, comment: "親科目のコード"
    t.datetime "updated_at", null: false
    t.index ["account_path"], name: "idx_rails_account_structure_path", comment: "パスでの検索を高速化"
    t.index ["hierarchy_level"], name: "idx_rails_account_structure_level", comment: "階層レベルでの検索を高速化"
    t.index ["parent_code"], name: "idx_rails_account_structure_parent", where: "(parent_code IS NOT NULL)", comment: "親科目での検索を高速化（部分インデックス）"
  end

  create_table "accounts", force: :cascade do |t|
    t.integer "account_type", default: 0, null: false
    t.decimal "balance", precision: 15, scale: 2, default: "0.0"
    t.string "bspl_type", limit: 1, comment: "BSPL区分（B:貸借対照表, P:損益計算書）"
    t.string "code", null: false
    t.datetime "created_at", null: false
    t.string "debit_credit_type", limit: 1, comment: "貸借区分（D:借方, C:貸方）"
    t.integer "display_order", comment: "表示順序"
    t.string "expense_type", limit: 1, comment: "費用区分（1:販管費, 2:営業外費用, 3:特別損失）"
    t.boolean "is_summary", default: false, null: false, comment: "集計科目フラグ（true:集計科目, false:明細科目）"
    t.string "name", null: false
    t.string "tax_code", limit: 2, comment: "課税取引コード（課税取引マスタへの外部キー）"
    t.string "transaction_type", limit: 1, comment: "取引要素区分（1:資産, 2:負債, 3:純資産, 4:収益, 5:費用）"
    t.datetime "updated_at", null: false
    t.index ["account_type"], name: "index_accounts_on_account_type"
    t.index ["bspl_type", "display_order"], name: "idx_accounts_on_bspl_type_and_display_order", where: "((bspl_type IS NOT NULL) AND (display_order IS NOT NULL))", comment: "BSPL区分での絞り込み + 表示順序でのソートを高速化（財務諸表表示用）"
    t.index ["bspl_type"], name: "idx_accounts_on_bspl_type", where: "(bspl_type IS NOT NULL)", comment: "BSPL区分での検索を高速化"
    t.index ["code"], name: "index_accounts_on_code", unique: true
    t.index ["display_order"], name: "idx_accounts_on_display_order", where: "(display_order IS NOT NULL)", comment: "表示順序でのソートを高速化"
    t.index ["tax_code"], name: "idx_accounts_on_tax_code", where: "(tax_code IS NOT NULL)", comment: "課税取引コードでの検索を高速化"
    t.index ["transaction_type"], name: "idx_accounts_on_transaction_type", where: "(transaction_type IS NOT NULL)", comment: "取引要素区分での検索を高速化"
    t.check_constraint "(bspl_type::text = ANY (ARRAY['B'::character varying::text, 'P'::character varying::text])) OR bspl_type IS NULL", name: "check_bspl_type"
    t.check_constraint "(expense_type::text = ANY (ARRAY['1'::character varying::text, '2'::character varying::text, '3'::character varying::text])) OR expense_type IS NULL", name: "check_expense_type"
    t.check_constraint "(transaction_type::text = ANY (ARRAY['1'::character varying::text, '2'::character varying::text, '3'::character varying::text, '4'::character varying::text, '5'::character varying::text])) OR transaction_type IS NULL", name: "check_transaction_type"
    t.check_constraint "bspl_type::text = 'B'::text AND (account_type = ANY (ARRAY[0, 1, 2])) OR bspl_type::text = 'P'::text AND (account_type = ANY (ARRAY[3, 4])) OR bspl_type IS NULL", name: "check_bspl_consistency"
    t.check_constraint "expense_type IS NOT NULL AND account_type = 4 OR expense_type IS NULL", name: "check_expense_type_only_for_expense"
  end

  create_table "audit_logs", comment: "監査ログテーブル（Append-Onlyで不変）", force: :cascade do |t|
    t.string "action", limit: 20, null: false, comment: "操作種別（create, update, delete）"
    t.jsonb "change_data", comment: "変更内容（CREATE時）"
    t.datetime "created_at", null: false
    t.string "entity_id", limit: 100, null: false, comment: "エンティティID"
    t.string "entity_type", limit: 50, null: false, comment: "エンティティ種別（Journal, Account等）"
    t.string "ip_address", limit: 45
    t.jsonb "new_values", comment: "変更後の値（UPDATE時）"
    t.jsonb "old_values", comment: "変更前の値（UPDATE, DELETE時）"
    t.text "reason", comment: "操作理由（任意）"
    t.datetime "timestamp", null: false
    t.datetime "updated_at", null: false
    t.text "user_agent"
    t.string "user_id", limit: 100, null: false
    t.string "user_name", limit: 200, null: false
    t.index ["action"], name: "idx_audit_log_action"
    t.index ["change_data"], name: "idx_audit_log_changes", using: :gin
    t.index ["entity_type", "entity_id"], name: "idx_audit_log_entity"
    t.index ["timestamp"], name: "idx_audit_log_timestamp"
    t.index ["user_id"], name: "idx_audit_log_user"
  end

  create_table "auto_journal_logs", force: :cascade do |t|
    t.bigint "auto_journal_pattern_id", null: false
    t.datetime "created_at", null: false
    t.text "error_detail"
    t.datetime "executed_at", precision: nil, null: false
    t.integer "generated_count", default: 0, null: false
    t.string "message", limit: 500
    t.integer "processed_count", default: 0, null: false
    t.string "status", limit: 20, null: false
    t.datetime "updated_at", null: false
    t.index ["auto_journal_pattern_id"], name: "index_auto_journal_logs_on_auto_journal_pattern_id"
    t.index ["executed_at"], name: "index_auto_journal_logs_on_executed_at"
    t.index ["status"], name: "index_auto_journal_logs_on_status"
  end

  create_table "auto_journal_managements", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.datetime "last_processed_at", precision: nil
    t.string "source_table_name", limit: 100, null: false
    t.datetime "updated_at", null: false
    t.index ["source_table_name"], name: "index_auto_journal_managements_on_source_table_name", unique: true
  end

  create_table "auto_journal_pattern_items", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false
    t.string "amount_expression", limit: 200, null: false
    t.bigint "auto_journal_pattern_id", null: false
    t.datetime "created_at", null: false
    t.string "debit_credit_flag", limit: 1, null: false
    t.string "description_template", limit: 200
    t.integer "line_number", null: false
    t.datetime "updated_at", null: false
    t.index ["auto_journal_pattern_id", "line_number"], name: "idx_auto_journal_pattern_items_unique", unique: true
    t.index ["auto_journal_pattern_id"], name: "index_auto_journal_pattern_items_on_auto_journal_pattern_id"
    t.check_constraint "debit_credit_flag::text = ANY (ARRAY['D'::character varying::text, 'C'::character varying::text])", name: "check_debit_credit_flag"
  end

  create_table "auto_journal_patterns", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.string "description", limit: 500
    t.boolean "is_active", default: true, null: false
    t.string "pattern_code", limit: 20, null: false
    t.string "pattern_name", limit: 100, null: false
    t.string "source_table_name", limit: 100, null: false
    t.datetime "updated_at", null: false
    t.index ["is_active"], name: "index_auto_journal_patterns_on_is_active"
    t.index ["pattern_code"], name: "index_auto_journal_patterns_on_pattern_code", unique: true
  end

  create_table "daily_account_balances", primary_key: ["entry_date", "account_code", "sub_account_code", "department_code", "project_code", "settlement_flag"], comment: "日次勘定科目残高テーブル", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false, comment: "勘定科目マスタの外部キー"
    t.datetime "created_at", null: false
    t.decimal "credit_amount", precision: 15, scale: 2, default: "0.0", null: false, comment: "貸方合計金額"
    t.decimal "debit_amount", precision: 15, scale: 2, default: "0.0", null: false, comment: "借方合計金額"
    t.string "department_code", limit: 5, default: "", null: false, comment: "部門別管理用"
    t.date "entry_date", null: false, comment: "実際の取引発生日"
    t.string "project_code", limit: 10, default: "", null: false, comment: "プロジェクト別管理用"
    t.integer "settlement_flag", default: 0, null: false, comment: "0=通常仕訳、1=決算仕訳"
    t.string "sub_account_code", limit: 10, default: "", null: false, comment: "補助科目（得意先、仕入先など）"
    t.datetime "updated_at", null: false
    t.index ["account_code"], name: "idx_daily_balance_account"
    t.index ["department_code"], name: "idx_daily_balance_department"
    t.index ["entry_date"], name: "idx_daily_balance_entry_date"
    t.index ["project_code"], name: "idx_daily_balance_project"
    t.check_constraint "credit_amount >= 0::numeric", name: "check_daily_balance_credit_amount"
    t.check_constraint "debit_amount >= 0::numeric", name: "check_daily_balance_debit_amount"
    t.check_constraint "settlement_flag = ANY (ARRAY[0, 1])", name: "check_daily_balance_settlement_flag"
  end

  create_table "event_stores", comment: "イベントストア（Event Sourcing用、Append-Onlyで不変）", force: :cascade do |t|
    t.string "aggregate_id", limit: 100, null: false, comment: "Aggregateのユニーク識別子（例: \"account-1001\"）"
    t.string "aggregate_type", limit: 50, null: false, comment: "Aggregateの型（例: \"Account\", \"JournalEntry\"）"
    t.string "causation_id", limit: 100, comment: "因果関係のあるイベントID"
    t.string "correlation_id", limit: 100, comment: "関連する一連のイベントをグループ化"
    t.datetime "created_at", null: false
    t.jsonb "event_data", null: false, comment: "イベントのペイロード（JSONB形式）"
    t.string "event_type", limit: 100, null: false, comment: "イベントの型（例: \"AccountCreatedEvent\"）"
    t.integer "event_version", default: 1, null: false, comment: "イベント定義のバージョン（スキーマ進化に対応）"
    t.datetime "occurred_at", default: -> { "CURRENT_TIMESTAMP" }, null: false, comment: "イベントが発生した日時"
    t.integer "sequence_number", null: false, comment: "Aggregate内でのイベントの順序番号"
    t.datetime "updated_at", null: false
    t.string "user_id", limit: 100, comment: "イベントを発生させたユーザーID"
    t.index ["aggregate_id", "sequence_number"], name: "idx_event_store_aggregate_id"
    t.index ["aggregate_id", "sequence_number"], name: "uk_aggregate_sequence", unique: true
    t.index ["correlation_id"], name: "idx_event_store_correlation_id"
    t.index ["event_data"], name: "idx_event_store_event_data", using: :gin
    t.index ["event_type"], name: "idx_event_store_event_type"
    t.index ["occurred_at"], name: "idx_event_store_occurred_at"
  end

  create_table "journal_detail_items", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false
    t.decimal "amount", precision: 15, scale: 2, null: false
    t.decimal "base_amount", precision: 15, scale: 2, null: false
    t.integer "cash_flow_flag", default: 0, null: false
    t.datetime "created_at", null: false
    t.string "currency_code", limit: 3, default: "JPY", null: false
    t.string "debit_credit_type", limit: 1, null: false
    t.string "department_code", limit: 5
    t.date "due_date"
    t.decimal "exchange_rate", precision: 10, scale: 4, default: "1.0", null: false
    t.bigint "journal_detail_id", null: false
    t.string "note_code", limit: 1
    t.string "note_content", limit: 60
    t.string "offset_account_code", limit: 10
    t.string "offset_sub_account_code", limit: 10
    t.string "project_code", limit: 10
    t.string "segment_code", limit: 10
    t.string "sub_account_code", limit: 10
    t.string "tax_calc_type", limit: 2
    t.integer "tax_rate"
    t.string "tax_type", limit: 2
    t.datetime "updated_at", null: false
    t.index ["account_code"], name: "index_journal_detail_items_on_account_code"
    t.index ["department_code"], name: "index_journal_detail_items_on_department_code"
    t.index ["journal_detail_id", "debit_credit_type"], name: "idx_journal_detail_items_unique", unique: true
    t.index ["journal_detail_id"], name: "index_journal_detail_items_on_journal_detail_id"
    t.index ["project_code"], name: "index_journal_detail_items_on_project_code"
    t.check_constraint "amount >= 0::numeric", name: "check_amount"
    t.check_constraint "debit_credit_type::text = ANY (ARRAY['D'::character varying::text, 'C'::character varying::text])", name: "check_debit_credit_type"
    t.check_constraint "exchange_rate > 0::numeric", name: "check_exchange_rate"
    t.check_constraint "length(currency_code::text) = 3", name: "check_currency_code_length"
  end

  create_table "journal_details", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.string "description", limit: 1000, null: false
    t.bigint "journal_id", null: false
    t.integer "line_number", null: false
    t.datetime "updated_at", null: false
    t.index ["journal_id", "line_number"], name: "index_journal_details_on_journal_id_and_line_number", unique: true
    t.index ["journal_id"], name: "index_journal_details_on_journal_id"
  end

  create_table "journal_entries", comment: "仕訳エントリ（複式簿記の仕訳データ）", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.string "created_by", limit: 20, null: false, comment: "作成者"
    t.string "description", limit: 100, null: false, comment: "摘要"
    t.date "entry_date", null: false, comment: "仕訳日"
    t.string "entry_number", limit: 10, null: false, comment: "伝票番号（主キー）"
    t.string "reference_number", limit: 20, comment: "参照番号"
    t.decimal "total_amount", precision: 15, scale: 2, null: false, comment: "合計金額"
    t.datetime "updated_at", null: false
    t.string "updated_by", limit: 20, comment: "更新者"
    t.index ["entry_date"], name: "index_journal_entries_on_entry_date"
    t.index ["entry_number"], name: "index_journal_entries_on_entry_number", unique: true
  end

  create_table "journal_entry_details", comment: "仕訳明細（仕訳エントリの明細行データ）", force: :cascade do |t|
    t.string "account_code", limit: 20, null: false, comment: "勘定科目コード"
    t.datetime "created_at", null: false
    t.decimal "credit_amount", precision: 15, scale: 2, default: "0.0", null: false, comment: "貸方金額"
    t.decimal "debit_amount", precision: 15, scale: 2, default: "0.0", null: false, comment: "借方金額"
    t.string "description", limit: 100, null: false, comment: "摘要"
    t.bigint "journal_entry_id", null: false, comment: "仕訳エントリID（外部キー）"
    t.integer "line_number", null: false, comment: "行番号"
    t.decimal "tax_amount", precision: 15, scale: 2, default: "0.0", comment: "消費税額"
    t.decimal "tax_rate", precision: 5, scale: 2, comment: "消費税率"
    t.datetime "updated_at", null: false
    t.index ["account_code"], name: "index_journal_entry_details_on_account_code"
    t.index ["journal_entry_id", "line_number"], name: "idx_on_journal_entry_id_line_number_2c8d7e16b4", unique: true
    t.index ["journal_entry_id"], name: "index_journal_entry_details_on_journal_entry_id"
  end

  create_table "journals", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.string "department_code", limit: 5
    t.string "employee_code", limit: 10
    t.date "input_date", null: false
    t.date "journal_date", null: false
    t.string "journal_no", limit: 20, null: false
    t.integer "periodic_flag", default: 0, null: false
    t.string "red_black_slip_number", limit: 20
    t.integer "red_slip_flag", default: 0, null: false
    t.integer "settlement_flag", default: 0, null: false
    t.integer "single_entry_flag", default: 0, null: false
    t.integer "slip_type", default: 0, null: false
    t.datetime "updated_at", null: false
    t.index ["department_code"], name: "index_journals_on_department_code"
    t.index ["journal_date"], name: "index_journals_on_journal_date"
    t.index ["journal_no"], name: "index_journals_on_journal_no", unique: true
    t.index ["red_slip_flag"], name: "index_journals_on_red_slip_flag"
    t.check_constraint "red_slip_flag = 0 OR red_slip_flag = 1 AND red_black_slip_number IS NOT NULL", name: "check_red_slip_voucher"
    t.check_constraint "red_slip_flag = ANY (ARRAY[0, 1])", name: "check_red_slip_flag"
    t.check_constraint "settlement_flag = ANY (ARRAY[0, 1])", name: "check_settlement_flag"
  end

  create_table "monthly_account_balances", primary_key: ["fiscal_year", "month", "account_code", "sub_account_code", "department_code", "project_code", "settlement_flag"], comment: "月次勘定科目残高テーブル", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false, comment: "勘定科目マスタの外部キー"
    t.decimal "beginning_balance", precision: 15, scale: 2, default: "0.0", null: false, comment: "月初時点の残高"
    t.datetime "created_at", null: false
    t.decimal "credit_amount", precision: 15, scale: 2, default: "0.0", null: false, comment: "貸方合計金額"
    t.decimal "debit_amount", precision: 15, scale: 2, default: "0.0", null: false, comment: "借方合計金額"
    t.string "department_code", limit: 5, default: "", null: false, comment: "部門別管理用"
    t.decimal "ending_balance", precision: 15, scale: 2, default: "0.0", null: false, comment: "月末時点の残高"
    t.integer "fiscal_year", null: false, comment: "会計年度（例：2025）"
    t.integer "month", null: false, comment: "月度（1～12）"
    t.string "project_code", limit: 10, default: "", null: false, comment: "プロジェクト別管理用"
    t.integer "settlement_flag", default: 0, null: false, comment: "0=通常仕訳、1=決算仕訳"
    t.string "sub_account_code", limit: 10, default: "", null: false, comment: "補助科目"
    t.datetime "updated_at", null: false
    t.index ["account_code"], name: "idx_monthly_balance_account"
    t.index ["department_code"], name: "idx_monthly_balance_department"
    t.index ["fiscal_year", "month"], name: "idx_monthly_balance_fiscal_month"
    t.index ["project_code"], name: "idx_monthly_balance_project"
    t.check_constraint "credit_amount >= 0::numeric", name: "check_monthly_balance_credit_amount"
    t.check_constraint "debit_amount >= 0::numeric", name: "check_monthly_balance_debit_amount"
    t.check_constraint "month >= 1 AND month <= 12", name: "check_monthly_balance_month_range"
    t.check_constraint "settlement_flag = ANY (ARRAY[0, 1])", name: "check_monthly_balance_settlement_flag"
  end

  add_foreign_key "account_structures", "accounts", column: "account_code", primary_key: "code", on_delete: :cascade
  add_foreign_key "auto_journal_logs", "auto_journal_patterns", on_delete: :cascade
  add_foreign_key "auto_journal_pattern_items", "auto_journal_patterns", on_delete: :cascade
  add_foreign_key "daily_account_balances", "accounts", column: "account_code", primary_key: "code"
  add_foreign_key "journal_detail_items", "journal_details", on_delete: :cascade
  add_foreign_key "journal_details", "journals", on_delete: :cascade
  add_foreign_key "journal_entry_details", "accounts", column: "account_code", primary_key: "code"
  add_foreign_key "journal_entry_details", "journal_entries", on_delete: :cascade
  add_foreign_key "monthly_account_balances", "accounts", column: "account_code", primary_key: "code"
end
