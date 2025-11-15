# frozen_string_literal: true

class CreatePayments < ActiveRecord::Migration[7.1]
  def change
    create_table :payments do |t|
      t.date :payment_date
      t.decimal :amount
      t.string :payment_method
      t.references :invoice, null: false, foreign_key: true

      t.timestamps
    end
  end
end
