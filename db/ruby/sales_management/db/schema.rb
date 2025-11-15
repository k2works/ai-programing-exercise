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

ActiveRecord::Schema[7.1].define(version: 2025_11_15_015416) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

  create_table "bill_items", force: :cascade do |t|
    t.bigint "bill_id", null: false
    t.bigint "purchase_id", null: false
    t.decimal "amount"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["bill_id"], name: "index_bill_items_on_bill_id"
    t.index ["purchase_id"], name: "index_bill_items_on_purchase_id"
  end

  create_table "bill_payments", force: :cascade do |t|
    t.date "payment_date"
    t.decimal "amount"
    t.string "payment_method"
    t.bigint "bill_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["bill_id"], name: "index_bill_payments_on_bill_id"
  end

  create_table "bills", force: :cascade do |t|
    t.string "bill_number"
    t.date "bill_date"
    t.date "closing_date"
    t.date "due_date"
    t.bigint "party_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["bill_number"], name: "index_bills_on_bill_number", unique: true
    t.index ["party_id"], name: "index_bills_on_party_id"
  end

  create_table "credit_limits", force: :cascade do |t|
    t.bigint "party_id", null: false
    t.decimal "limit_amount"
    t.decimal "used_amount"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["party_id"], name: "index_credit_limits_on_party_id"
  end

  create_table "departments", force: :cascade do |t|
    t.string "code"
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["code"], name: "index_departments_on_code", unique: true
  end

  create_table "employees", force: :cascade do |t|
    t.string "code"
    t.string "first_name"
    t.string "last_name"
    t.bigint "department_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["code"], name: "index_employees_on_code", unique: true
    t.index ["department_id"], name: "index_employees_on_department_id"
  end

  create_table "invoice_items", force: :cascade do |t|
    t.bigint "invoice_id", null: false
    t.bigint "order_id", null: false
    t.decimal "amount"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["invoice_id"], name: "index_invoice_items_on_invoice_id"
    t.index ["order_id"], name: "index_invoice_items_on_order_id"
  end

  create_table "invoices", force: :cascade do |t|
    t.string "invoice_number"
    t.date "invoice_date"
    t.date "closing_date"
    t.date "due_date"
    t.bigint "party_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["invoice_number"], name: "index_invoices_on_invoice_number", unique: true
    t.index ["party_id"], name: "index_invoices_on_party_id"
  end

  create_table "number_sequences", force: :cascade do |t|
    t.string "sequence_type"
    t.string "prefix"
    t.integer "current_number"
    t.date "last_generated_date"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["sequence_type"], name: "index_number_sequences_on_sequence_type", unique: true
  end

  create_table "order_items", force: :cascade do |t|
    t.bigint "order_id", null: false
    t.bigint "product_id", null: false
    t.integer "quantity"
    t.decimal "unit_price"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["order_id"], name: "index_order_items_on_order_id"
    t.index ["product_id"], name: "index_order_items_on_product_id"
  end

  create_table "orders", force: :cascade do |t|
    t.string "order_number"
    t.string "order_type"
    t.date "order_date"
    t.string "status"
    t.bigint "party_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["order_number"], name: "index_orders_on_order_number", unique: true
    t.index ["party_id"], name: "index_orders_on_party_id"
  end

  create_table "organizations", force: :cascade do |t|
    t.bigint "party_id", null: false
    t.string "name"
    t.string "tax_id"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["party_id"], name: "index_organizations_on_party_id"
  end

  create_table "parties", force: :cascade do |t|
    t.string "party_type"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "party_roles", force: :cascade do |t|
    t.bigint "party_id", null: false
    t.string "role_type"
    t.datetime "started_at"
    t.datetime "ended_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["party_id"], name: "index_party_roles_on_party_id"
  end

  create_table "payments", force: :cascade do |t|
    t.date "payment_date"
    t.decimal "amount"
    t.string "payment_method"
    t.bigint "invoice_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["invoice_id"], name: "index_payments_on_invoice_id"
  end

  create_table "people", force: :cascade do |t|
    t.bigint "party_id", null: false
    t.string "first_name"
    t.string "last_name"
    t.string "email"
    t.string "phone"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["party_id"], name: "index_people_on_party_id"
  end

  create_table "product_categories", force: :cascade do |t|
    t.string "code"
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["code"], name: "index_product_categories_on_code", unique: true
  end

  create_table "products", force: :cascade do |t|
    t.string "code"
    t.string "name"
    t.decimal "unit_price"
    t.bigint "product_category_id", null: false
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["code"], name: "index_products_on_code", unique: true
    t.index ["product_category_id"], name: "index_products_on_product_category_id"
  end

  create_table "purchase_items", force: :cascade do |t|
    t.bigint "purchase_id", null: false
    t.bigint "product_id", null: false
    t.string "lot_number"
    t.bigint "warehouse_id", null: false
    t.integer "quantity"
    t.decimal "unit_price"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["product_id"], name: "index_purchase_items_on_product_id"
    t.index ["purchase_id"], name: "index_purchase_items_on_purchase_id"
    t.index ["warehouse_id"], name: "index_purchase_items_on_warehouse_id"
  end

  create_table "purchase_order_items", force: :cascade do |t|
    t.bigint "purchase_order_id", null: false
    t.bigint "product_id", null: false
    t.integer "quantity"
    t.integer "received_quantity"
    t.decimal "unit_price"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["product_id"], name: "index_purchase_order_items_on_product_id"
    t.index ["purchase_order_id"], name: "index_purchase_order_items_on_purchase_order_id"
  end

  create_table "purchase_orders", force: :cascade do |t|
    t.string "order_number"
    t.date "order_date"
    t.bigint "party_id", null: false
    t.bigint "warehouse_id", null: false
    t.bigint "sales_order_id"
    t.string "status"
    t.decimal "total_amount"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["order_number"], name: "index_purchase_orders_on_order_number", unique: true
    t.index ["party_id"], name: "index_purchase_orders_on_party_id"
    t.index ["sales_order_id"], name: "index_purchase_orders_on_sales_order_id"
    t.index ["warehouse_id"], name: "index_purchase_orders_on_warehouse_id"
  end

  create_table "purchases", force: :cascade do |t|
    t.string "purchase_number"
    t.date "purchase_date"
    t.bigint "purchase_order_id", null: false
    t.bigint "party_id", null: false
    t.decimal "total_amount"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["party_id"], name: "index_purchases_on_party_id"
    t.index ["purchase_number"], name: "index_purchases_on_purchase_number", unique: true
    t.index ["purchase_order_id"], name: "index_purchases_on_purchase_order_id"
  end

  create_table "samples", force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  create_table "stocks", primary_key: ["warehouse_id", "product_id", "lot_number", "stock_type", "quality_type"], force: :cascade do |t|
    t.bigint "warehouse_id", null: false
    t.bigint "product_id", null: false
    t.string "lot_number", null: false
    t.string "stock_type", null: false
    t.string "quality_type", null: false
    t.integer "actual_quantity", default: 0, null: false
    t.integer "valid_quantity", default: 0, null: false
    t.datetime "last_shipped_at"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["product_id"], name: "index_stocks_on_product_id"
    t.index ["warehouse_id"], name: "index_stocks_on_warehouse_id"
  end

  create_table "warehouses", force: :cascade do |t|
    t.string "code"
    t.string "name"
    t.integer "warehouse_type"
    t.string "address"
    t.string "phone"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
    t.index ["code"], name: "index_warehouses_on_code", unique: true
  end

  add_foreign_key "bill_items", "bills"
  add_foreign_key "bill_items", "purchases"
  add_foreign_key "bill_payments", "bills"
  add_foreign_key "bills", "parties"
  add_foreign_key "credit_limits", "parties"
  add_foreign_key "employees", "departments"
  add_foreign_key "invoice_items", "invoices"
  add_foreign_key "invoice_items", "orders"
  add_foreign_key "invoices", "parties"
  add_foreign_key "order_items", "orders"
  add_foreign_key "order_items", "products"
  add_foreign_key "orders", "parties"
  add_foreign_key "organizations", "parties"
  add_foreign_key "party_roles", "parties"
  add_foreign_key "payments", "invoices"
  add_foreign_key "people", "parties"
  add_foreign_key "products", "product_categories"
  add_foreign_key "purchase_items", "products"
  add_foreign_key "purchase_items", "purchases"
  add_foreign_key "purchase_items", "warehouses"
  add_foreign_key "purchase_order_items", "products"
  add_foreign_key "purchase_order_items", "purchase_orders"
  add_foreign_key "purchase_orders", "orders", column: "sales_order_id"
  add_foreign_key "purchase_orders", "parties"
  add_foreign_key "purchase_orders", "warehouses"
  add_foreign_key "purchases", "parties"
  add_foreign_key "purchases", "purchase_orders"
  add_foreign_key "stocks", "products"
  add_foreign_key "stocks", "warehouses"
end
