# frozen_string_literal: true

class CreateAccounts < ActiveRecord::Migration[8.1]
  def change
    create_table :accounts do |t|
      t.string :code, null: false
      t.string :name, null: false
      t.integer :account_type, null: false, default: 0
      t.decimal :balance, precision: 15, scale: 2, default: 0.0

      t.timestamps
    end

    add_index :accounts, :code, unique: true
    add_index :accounts, :account_type
  end
end
