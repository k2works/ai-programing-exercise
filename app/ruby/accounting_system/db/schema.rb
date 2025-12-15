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

ActiveRecord::Schema[8.1].define(version: 2025_12_15_002323) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "pg_catalog.plpgsql"

  # Custom types defined in this database.
  # Note that some types may not work with other database engines. Be careful if changing database.
  create_enum "account_type", ["資産", "負債", "純資産", "収益", "費用"]

  create_table "_sqlx_migrations", primary_key: "version", id: :bigint, default: nil, force: :cascade do |t|
    t.binary "checksum", null: false
    t.text "description", null: false
    t.bigint "execution_time", null: false
    t.timestamptz "installed_on", default: -> { "now()" }, null: false
    t.boolean "success", null: false
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
    t.string "transaction_type", limit: 1, comment: "取引要素区分（1:資産, 2:負債, 3:純資産, 4:収益, 5:費用）"
    t.datetime "updated_at", null: false
    t.index ["account_type"], name: "index_accounts_on_account_type"
    t.index ["code"], name: "index_accounts_on_code", unique: true
  end

  create_table "audit_log", comment: "監査ログテーブル（Append-Onlyで不変）", force: :cascade do |t|
    t.string "action", limit: 20, null: false, comment: "操作種別（CREATE/UPDATE/DELETE）"
    t.jsonb "changes", comment: "変更された項目のみ（JSON形式）"
    t.timestamptz "created_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.string "entity_id", limit: 100, null: false, comment: "監査対象エンティティID"
    t.string "entity_type", limit: 50, null: false, comment: "監査対象エンティティタイプ（例：Account, Journal）"
    t.string "ip_address", limit: 45
    t.jsonb "new_values", comment: "変更後の値（JSON形式）"
    t.jsonb "old_values", comment: "変更前の値（JSON形式）"
    t.text "reason"
    t.timestamptz "timestamp", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.text "user_agent"
    t.string "user_id", limit: 100, null: false
    t.string "user_name", limit: 200, null: false
    t.index ["action"], name: "idx_audit_log_action"
    t.index ["changes"], name: "idx_audit_log_changes", using: :gin
    t.index ["entity_type", "entity_id"], name: "idx_audit_log_entity"
    t.index ["timestamp"], name: "idx_audit_log_timestamp", order: :desc
    t.index ["user_id"], name: "idx_audit_log_user"
    t.check_constraint "action::text = ANY (ARRAY['CREATE'::character varying::text, 'UPDATE'::character varying::text, 'DELETE'::character varying::text])", name: "audit_log_action_check"
  end

  create_table "event_store", primary_key: "sequence_number", id: { comment: "グローバルなイベントシーケンス番号" }, comment: "イベントソーシング用の追記専用イベントストア", force: :cascade do |t|
    t.string "aggregate_id", limit: 255, null: false, comment: "集約の一意識別子"
    t.string "aggregate_type", limit: 100, null: false, comment: "集約のタイプ（例：Journal, Account）"
    t.uuid "causation_id", comment: "このイベントの原因となったイベントのID"
    t.uuid "correlation_id", comment: "関連するイベント群を追跡するID"
    t.jsonb "event_data", null: false, comment: "イベントデータ（JSON形式）"
    t.string "event_type", limit: 100, null: false, comment: "イベントのタイプ名"
    t.integer "event_version", default: 1, null: false, comment: "イベントスキーマのバージョン"
    t.jsonb "metadata"
    t.timestamptz "occurred_at", default: -> { "now()" }, null: false
    t.string "user_id", limit: 100
    t.index ["aggregate_type", "aggregate_id", "sequence_number"], name: "idx_event_store_aggregate"
    t.index ["correlation_id"], name: "idx_event_store_correlation", where: "(correlation_id IS NOT NULL)"
    t.index ["event_data"], name: "idx_event_store_data", using: :gin
    t.index ["event_type"], name: "idx_event_store_event_type"
    t.index ["occurred_at"], name: "idx_event_store_occurred_at"
    t.unique_constraint ["aggregate_type", "aggregate_id", "sequence_number"], name: "unique_aggregate_sequence"
  end

  create_table "journal_entry_read_model", primary_key: "entry_id", comment: "CQRS読み取りモデル：仕訳明細表示用", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false
    t.string "account_name", limit: 100
    t.timestamptz "created_at", default: -> { "now()" }, null: false
    t.bigint "credit_amount", comment: "貸方金額（貸方の場合のみ）"
    t.bigint "debit_amount", comment: "借方金額（借方の場合のみ）"
    t.text "description"
    t.string "journal_id", limit: 255, null: false
    t.integer "line_number", null: false
    t.index ["account_code"], name: "idx_entry_read_account"
    t.index ["journal_id"], name: "idx_entry_read_journal"
  end

  create_table "journal_read_model", primary_key: "journal_id", id: { type: :string, limit: 255, comment: "仕訳ID（集約ID）" }, comment: "CQRS読み取りモデル：仕訳一覧・検索用", force: :cascade do |t|
    t.timestamptz "approved_at"
    t.string "approved_by", limit: 100
    t.timestamptz "created_at", default: -> { "now()" }, null: false
    t.text "description", null: false
    t.integer "entry_count", default: 0, null: false, comment: "明細数"
    t.integer "fiscal_year", null: false
    t.date "journal_date", null: false
    t.string "status", limit: 20, null: false, comment: "ステータス（draft/approved/canceled）"
    t.bigint "total_credit", default: 0, null: false, comment: "貸方合計"
    t.bigint "total_debit", default: 0, null: false, comment: "借方合計"
    t.timestamptz "updated_at", default: -> { "now()" }, null: false
    t.index ["created_at"], name: "idx_journal_read_created_at", order: :desc
    t.index ["fiscal_year"], name: "idx_journal_read_fiscal_year"
    t.index ["journal_date"], name: "idx_journal_read_date"
    t.index ["status"], name: "idx_journal_read_status"
  end

  create_table "samples", force: :cascade do |t|
    t.datetime "created_at", null: false
    t.string "name"
    t.datetime "updated_at", null: false
  end

  create_table "snapshots", primary_key: "snapshot_id", comment: "イベントソーシング用スナップショット", force: :cascade do |t|
    t.string "aggregate_id", limit: 255, null: false, comment: "集約の一意識別子"
    t.string "aggregate_type", limit: 100, null: false, comment: "集約のタイプ（例：Journal, Account）"
    t.timestamptz "created_at", default: -> { "now()" }, null: false
    t.jsonb "snapshot_data", null: false, comment: "集約の状態（JSON形式）"
    t.bigint "version", null: false, comment: "スナップショット作成時点のイベントバージョン（シーケンス番号）"
    t.index ["aggregate_type", "aggregate_id", "version"], name: "idx_snapshots_version", order: { version: :desc }
    t.index ["aggregate_type", "aggregate_id"], name: "idx_snapshots_aggregate"
    t.unique_constraint ["aggregate_type", "aggregate_id"], name: "snapshots_aggregate_type_aggregate_id_key"
  end

  create_table "仕訳", primary_key: "仕訳伝票番号", id: { type: :string, limit: 20 }, comment: "仕訳ヘッダー（伝票単位の基本情報）", force: :cascade do |t|
    t.integer "仕訳伝票区分", default: 0, null: false
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.date "入力日", null: false, comment: "システムへの入力日"
    t.integer "単振フラグ", default: 0, null: false, comment: "0=複合仕訳、1=単一仕訳"
    t.integer "定期計上フラグ", default: 0, null: false
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.integer "決算仕訳フラグ", default: 0, null: false, comment: "0=通常仕訳、1=決算仕訳"
    t.string "社員コード", limit: 10
    t.integer "赤伝フラグ", default: 0, null: false, comment: "0=通常、1=赤伝票（取消仕訳）"
    t.string "赤黒伝票番号", limit: 20
    t.date "起票日", null: false, comment: "実際の取引発生日"
    t.string "部門コード", limit: 5
    t.index ["赤伝フラグ"], name: "idx_仕訳_赤伝フラグ"
    t.index ["起票日"], name: "idx_仕訳_起票日"
    t.index ["部門コード"], name: "idx_仕訳_部門コード"
  end

  create_table "仕訳明細", primary_key: ["仕訳伝票番号", "仕訳行番号"], comment: "仕訳明細（行単位の情報）", force: :cascade do |t|
    t.string "仕訳伝票番号", limit: 20, null: false
    t.integer "仕訳行番号", null: false
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.string "行摘要", limit: 1000, null: false
    t.index ["仕訳伝票番号"], name: "idx_仕訳明細_伝票番号"
  end

  create_table "仕訳貸借明細", primary_key: ["仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分"], comment: "仕訳貸借明細（借方・貸方の詳細情報）", force: :cascade do |t|
    t.string "セグメントコード", limit: 10
    t.string "プロジェクトコード", limit: 10
    t.string "仕訳伝票番号", limit: 20, null: false
    t.integer "仕訳行番号", null: false
    t.string "仕訳行貸借区分", limit: 1, null: false, comment: "D=借方（Debit）、C=貸方（Credit）"
    t.decimal "仕訳金額", precision: 15, scale: 2, null: false
    t.string "付箋コード", limit: 1
    t.string "付箋内容", limit: 60
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.string "勘定科目コード", limit: 10, null: false
    t.decimal "基軸換算仕訳金額", precision: 15, scale: 2, null: false
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.date "期日"
    t.string "消費税区分", limit: 2
    t.integer "消費税率"
    t.string "消費税計算区分", limit: 2
    t.decimal "為替レート", precision: 10, scale: 4, default: "1.0", null: false
    t.string "相手勘定科目コード", limit: 10
    t.string "相手補助科目コード", limit: 10
    t.string "補助科目コード", limit: 10
    t.integer "資金繰フラグ", default: 0, null: false
    t.string "通貨コード", limit: 3, default: "JPY", null: false
    t.string "部門コード", limit: 5
    t.index ["プロジェクトコード"], name: "idx_仕訳貸借明細_プロジェクト"
    t.index ["仕訳伝票番号"], name: "idx_仕訳貸借明細_伝票番号"
    t.index ["勘定科目コード"], name: "idx_仕訳貸借明細_勘定科目"
    t.index ["部門コード"], name: "idx_仕訳貸借明細_部門"
    t.check_constraint "\"仕訳行貸借区分\"::bpchar = ANY (ARRAY['D'::bpchar, 'C'::bpchar])", name: "check_貸借区分"
    t.check_constraint "\"仕訳金額\" >= 0::numeric", name: "check_仕訳金額"
    t.check_constraint "\"為替レート\" > 0::numeric", name: "check_為替レート"
    t.check_constraint "length(\"通貨コード\"::text) = 3", name: "check_通貨コード長"
  end

  create_table "勘定科目マスタ", primary_key: "勘定科目ID", id: { type: :serial, comment: "勘定科目ID（主キー）" }, comment: "勘定科目マスタ（財務会計システムの基本となる勘定科目情報）", force: :cascade do |t|
    t.string "BSPL区分", limit: 1, comment: "BSPL区分（B:貸借対照表, P:損益計算書）"
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false, comment: "作成日時"
    t.string "勘定科目カナ", limit: 100, comment: "勘定科目名のカナ表記"
    t.string "勘定科目コード", limit: 20, null: false, comment: "勘定科目コード（例：1000, 2000）"
    t.string "勘定科目名", limit: 100, null: false, comment: "勘定科目名（例：現金、売掛金）"
    t.enum "勘定科目種別", null: false, comment: "勘定科目種別（資産、負債、純資産、収益、費用）", enum_type: "account_type"
    t.string "取引要素区分", limit: 10, comment: "取引要素区分（資金/損益/etc）"
    t.boolean "合計科目", default: false, null: false, comment: "合計科目かどうか（TRUE: 合計科目、FALSE: 明細科目）"
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false, comment: "更新日時"
    t.decimal "残高", precision: 15, scale: 2, default: "0.0", null: false, comment: "残高"
    t.integer "表示順序", default: 0, null: false, comment: "勘定科目の表示順序（昇順）"
    t.string "課税取引コード", limit: 2, comment: "課税取引コード（課税取引マスタへの外部キー）"
    t.string "貸借区分", limit: 1, comment: "貸借区分（D:借方, C:貸方）"
    t.string "費用区分", limit: 10, comment: "費用区分（製造原価/販管費/営業外/etc）"
    t.string "集計区分", limit: 1, comment: "集計区分（H:見出科目, S:集計科目, D:明細科目）"
    t.boolean "集計対象", default: true, null: false, comment: "集計対象かどうか（TRUE: 集計対象、FALSE: 集計対象外）"
    t.index ["勘定科目コード"], name: "idx_勘定科目マスタ_勘定科目コード"
    t.index ["勘定科目種別"], name: "idx_勘定科目マスタ_勘定科目種別"
    t.index ["表示順序"], name: "idx_account_display_order", comment: "表示順序での並び替えを高速化"
    t.index ["課税取引コード"], name: "idx_account_tax_code", where: "(\"課税取引コード\" IS NOT NULL)", comment: "課税取引コードでの検索を高速化（部分インデックス）"
    t.check_constraint "(\"BSPL区分\"::bpchar = ANY (ARRAY['B'::bpchar, 'P'::bpchar])) OR \"BSPL区分\" IS NULL", name: "check_bspl_distinction"
    t.check_constraint "(\"貸借区分\"::bpchar = ANY (ARRAY['D'::bpchar, 'C'::bpchar])) OR \"貸借区分\" IS NULL", name: "check_debit_credit_distinction"
    t.check_constraint "(\"集計区分\"::bpchar = ANY (ARRAY['H'::bpchar, 'S'::bpchar, 'D'::bpchar])) OR \"集計区分\" IS NULL", name: "check_aggregation_distinction"
    t.check_constraint "\"BSPL区分\"::bpchar = 'B'::bpchar AND (\"勘定科目種別\"::text = ANY (ARRAY['資産'::text, '負債'::text, '純資産'::text])) OR \"BSPL区分\"::bpchar = 'P'::bpchar AND (\"勘定科目種別\"::text = ANY (ARRAY['収益'::text, '費用'::text])) OR \"BSPL区分\" IS NULL", name: "check_bspl_consistency"
    t.check_constraint "\"貸借区分\"::bpchar = 'D'::bpchar AND (\"勘定科目種別\"::text = ANY (ARRAY['資産'::text, '費用'::text])) OR \"貸借区分\"::bpchar = 'C'::bpchar AND (\"勘定科目種別\"::text = ANY (ARRAY['負債'::text, '純資産'::text, '収益'::text])) OR \"貸借区分\" IS NULL", name: "check_debit_credit_consistency"
    t.unique_constraint ["勘定科目コード"], name: "勘定科目マスタ_勘定科目コード_key"
  end

  create_table "勘定科目構成マスタ", primary_key: "勘定科目コード", id: { type: :string, limit: 20, comment: "勘定科目コード" }, comment: "勘定科目の階層構造を管理するマスタテーブル", force: :cascade do |t|
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.string "勘定科目パス", limit: 200, null: false, comment: "チルダ連結形式のパス（例: 11~11000~11190~11110）"
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.integer "表示順序", default: 0, null: false, comment: "同じ階層内での表示順序"
    t.string "親科目コード", limit: 20, comment: "親科目のコード"
    t.integer "階層レベル", default: 1, null: false, comment: "階層の深さ（ルート=1）"
    t.index ["勘定科目パス"], name: "idx_account_structure_path", comment: "パスでの検索を高速化"
    t.index ["親科目コード"], name: "idx_account_structure_parent", where: "(\"親科目コード\" IS NOT NULL)", comment: "親科目での検索を高速化（部分インデックス）"
    t.index ["階層レベル"], name: "idx_account_structure_level", comment: "階層レベルでの検索を高速化"
  end

  create_table "日次勘定科目残高", primary_key: ["起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ"], comment: "日次勘定科目残高（日ごとの借方・貸方金額を記録）", force: :cascade do |t|
    t.string "プロジェクトコード", limit: 10, default: "", null: false, comment: "プロジェクト別管理用"
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.decimal "借方金額", precision: 15, scale: 2, default: "0.0", null: false, comment: "借方合計金額"
    t.string "勘定科目コード", limit: 10, null: false, comment: "勘定科目マスタの外部キー"
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.integer "決算仕訳フラグ", default: 0, null: false, comment: "0=通常仕訳、1=決算仕訳"
    t.string "補助科目コード", limit: 10, default: "", null: false, comment: "補助科目（得意先、仕入先など）"
    t.decimal "貸方金額", precision: 15, scale: 2, default: "0.0", null: false, comment: "貸方合計金額"
    t.date "起票日", null: false, comment: "実際の取引発生日"
    t.string "部門コード", limit: 5, default: "", null: false, comment: "部門別管理用"
    t.index ["プロジェクトコード"], name: "idx_日次勘定科目残高_プロジェクト"
    t.index ["勘定科目コード"], name: "idx_日次勘定科目残高_勘定科目"
    t.index ["起票日"], name: "idx_日次勘定科目残高_起票日"
    t.index ["部門コード"], name: "idx_日次勘定科目残高_部門"
    t.check_constraint "\"借方金額\" >= 0::numeric", name: "check_日次残高_借方金額"
    t.check_constraint "\"決算仕訳フラグ\" = ANY (ARRAY[0, 1])", name: "check_日次残高_決算仕訳フラグ"
    t.check_constraint "\"貸方金額\" >= 0::numeric", name: "check_日次残高_貸方金額"
  end

  create_table "月次勘定科目残高", primary_key: ["決算期", "月度", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ"], comment: "月次勘定科目残高（月ごとの月初残高・借方金額・貸方金額・月末残高を記録）", force: :cascade do |t|
    t.string "プロジェクトコード", limit: 10, default: "", null: false
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.decimal "借方金額", precision: 15, scale: 2, default: "0.0", null: false, comment: "当月の借方合計金額"
    t.string "勘定科目コード", limit: 10, null: false
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.decimal "月初残高", precision: 15, scale: 2, default: "0.0", null: false, comment: "月初時点の残高"
    t.integer "月度", null: false, comment: "会計月度（1〜12）"
    t.decimal "月末残高", precision: 15, scale: 2, default: "0.0", null: false, comment: "月末時点の残高（月初残高 + 借方金額 - 貸方金額）"
    t.integer "決算仕訳フラグ", default: 0, null: false
    t.integer "決算期", null: false, comment: "会計年度（例：2025）"
    t.string "補助科目コード", limit: 10, default: "", null: false
    t.decimal "貸方金額", precision: 15, scale: 2, default: "0.0", null: false, comment: "当月の貸方合計金額"
    t.string "部門コード", limit: 5, default: "", null: false
    t.index ["プロジェクトコード"], name: "idx_月次勘定科目残高_プロジェクト"
    t.index ["勘定科目コード"], name: "idx_月次勘定科目残高_勘定科目"
    t.index ["決算期", "月度"], name: "idx_月次勘定科目残高_決算期月度"
    t.index ["部門コード"], name: "idx_月次勘定科目残高_部門"
    t.check_constraint "\"月度\" >= 1 AND \"月度\" <= 12", name: "check_月次残高_月度"
    t.check_constraint "\"決算仕訳フラグ\" = ANY (ARRAY[0, 1])", name: "check_月次残高_決算仕訳フラグ"
  end

  create_table "自動仕訳パターン", comment: "自動仕訳の生成パターンを定義するテーブル", force: :cascade do |t|
    t.timestamptz "created_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.string "description", limit: 500
    t.boolean "is_active", default: true, null: false, comment: "有効/無効フラグ"
    t.string "pattern_code", limit: 20, null: false, comment: "パターン識別コード"
    t.string "pattern_name", limit: 100, null: false
    t.string "source_table_name", limit: 100, null: false
    t.timestamptz "updated_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.index ["is_active"], name: "idx_自動仕訳パターン_is_active"
    t.index ["pattern_code"], name: "idx_自動仕訳パターン_pattern_code"
    t.unique_constraint ["pattern_code"], name: "自動仕訳パターン_pattern_code_key"
  end

  create_table "自動仕訳パターン明細", comment: "自動仕訳パターンの明細（勘定科目と金額式）", force: :cascade do |t|
    t.string "account_code", limit: 10, null: false
    t.string "amount_expression", limit: 200, null: false, comment: "金額計算式（例: total_amount, price * quantity）"
    t.timestamptz "created_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.string "debit_credit_flag", limit: 1, null: false
    t.string "description_template", limit: 200
    t.integer "line_number", null: false
    t.bigint "pattern_id", null: false
    t.timestamptz "updated_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.index ["pattern_id"], name: "idx_自動仕訳パターン明細_pattern_id"
    t.check_constraint "debit_credit_flag::bpchar = ANY (ARRAY['D'::bpchar, 'C'::bpchar])", name: "check_貸借区分_auto"
    t.unique_constraint ["pattern_id", "line_number", "debit_credit_flag"], name: "uk_自動仕訳パターン明細_pattern_line"
  end

  create_table "自動仕訳実行ログ", comment: "自動仕訳の実行履歴と監査証跡", force: :cascade do |t|
    t.timestamptz "created_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.text "error_detail"
    t.datetime "executed_at", precision: nil, null: false
    t.integer "generated_count", default: 0, null: false, comment: "生成仕訳件数"
    t.string "message", limit: 500
    t.bigint "pattern_id", null: false
    t.integer "processed_count", default: 0, null: false, comment: "処理対象データ件数"
    t.string "status", limit: 20, null: false, comment: "実行ステータス（success, error, running）"
    t.index ["executed_at"], name: "idx_自動仕訳実行ログ_executed_at"
    t.index ["pattern_id"], name: "idx_自動仕訳実行ログ_pattern_id"
    t.index ["status"], name: "idx_自動仕訳実行ログ_status"
    t.check_constraint "generated_count >= 0", name: "check_generated_count"
    t.check_constraint "processed_count >= 0", name: "check_processed_count"
    t.check_constraint "status::text = ANY (ARRAY['success'::character varying::text, 'error'::character varying::text, 'running'::character varying::text])", name: "check_status"
  end

  create_table "自動仕訳管理", comment: "自動仕訳の最終処理日時を管理するテーブル（日付管理方式）", force: :cascade do |t|
    t.timestamptz "created_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.datetime "last_processed_at", precision: nil, null: false, comment: "最終処理日時"
    t.string "source_table_name", limit: 100, null: false, comment: "連携元テーブル名（一意）"
    t.timestamptz "updated_at", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.index ["source_table_name"], name: "idx_自動仕訳管理_source_table"
    t.unique_constraint ["source_table_name"], name: "自動仕訳管理_source_table_name_key"
  end

  create_table "課税取引マスタ", primary_key: "課税取引コード", id: { type: :string, limit: 2, comment: "課税取引コード（01:課税、02:非課税、03:免税、04:不課税）" }, comment: "消費税の課税取引区分を管理するマスタテーブル", force: :cascade do |t|
    t.timestamptz "作成日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.timestamptz "更新日時", default: -> { "CURRENT_TIMESTAMP" }, null: false
    t.boolean "有効フラグ", default: true, null: false, comment: "有効な課税取引区分かどうか"
    t.decimal "税率", precision: 5, scale: 3, default: "0.0", null: false, comment: "適用される税率（0.10 = 10%）"
    t.string "説明", limit: 200, comment: "課税取引の説明"
    t.string "課税取引名", limit: 20, null: false, comment: "課税取引名"
    t.check_constraint "\"税率\" >= 0::numeric AND \"税率\" <= 1::numeric", name: "check_tax_rate"
  end

  add_foreign_key "journal_entry_read_model", "journal_read_model", column: "journal_id", primary_key: "journal_id", name: "journal_entry_read_model_journal_id_fkey", on_delete: :cascade
  add_foreign_key "仕訳明細", "仕訳", column: "仕訳伝票番号", primary_key: "仕訳伝票番号", name: "fk_仕訳明細_仕訳", on_delete: :cascade
  add_foreign_key "仕訳貸借明細", "仕訳明細", column: ["仕訳伝票番号", "仕訳行番号"], primary_key: ["仕訳伝票番号", "仕訳行番号"], name: "fk_仕訳貸借明細_仕訳明細", on_delete: :cascade
  add_foreign_key "仕訳貸借明細", "勘定科目マスタ", column: "勘定科目コード", primary_key: "勘定科目コード", name: "fk_仕訳貸借明細_勘定科目マスタ"
  add_foreign_key "勘定科目マスタ", "課税取引マスタ", column: "課税取引コード", primary_key: "課税取引コード", name: "fk_account_tax_code", on_delete: :nullify
  add_foreign_key "勘定科目構成マスタ", "勘定科目マスタ", column: "勘定科目コード", primary_key: "勘定科目コード", name: "fk_account_structure_account", on_delete: :cascade
  add_foreign_key "日次勘定科目残高", "勘定科目マスタ", column: "勘定科目コード", primary_key: "勘定科目コード", name: "fk_日次勘定科目残高_勘定科目マスタ"
  add_foreign_key "月次勘定科目残高", "勘定科目マスタ", column: "勘定科目コード", primary_key: "勘定科目コード", name: "fk_月次勘定科目残高_勘定科目マスタ"
  add_foreign_key "自動仕訳パターン明細", "自動仕訳パターン", column: "pattern_id", name: "fk_自動仕訳パターン明細_パターン", on_delete: :cascade
  add_foreign_key "自動仕訳実行ログ", "自動仕訳パターン", column: "pattern_id", name: "fk_自動仕訳実行ログ_パターン", on_delete: :cascade
end
