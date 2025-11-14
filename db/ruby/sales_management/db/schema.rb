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

ActiveRecord::Schema[7.1].define(version: 2025_11_14_080409) do
  # These are extensions that must be enabled in order to support this database
  enable_extension "plpgsql"

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

  create_table "samples", force: :cascade do |t|
    t.string "name"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end

  add_foreign_key "employees", "departments"
  add_foreign_key "organizations", "parties"
  add_foreign_key "party_roles", "parties"
  add_foreign_key "people", "parties"
  add_foreign_key "products", "product_categories"
end
