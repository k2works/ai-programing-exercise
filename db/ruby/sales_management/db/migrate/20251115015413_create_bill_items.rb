# frozen_string_literal: true

class CreateBillItems < ActiveRecord::Migration[7.1]
  def change
    create_table :bill_items do |t|
      t.references :bill, null: false, foreign_key: true
      t.references :purchase, null: false, foreign_key: true
      t.decimal :amount

      t.timestamps
    end
  end
end
