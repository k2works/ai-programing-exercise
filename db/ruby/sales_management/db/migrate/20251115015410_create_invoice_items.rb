# frozen_string_literal: true

class CreateInvoiceItems < ActiveRecord::Migration[7.1]
  def change
    create_table :invoice_items do |t|
      t.references :invoice, null: false, foreign_key: true
      t.references :order, null: false, foreign_key: true
      t.decimal :amount

      t.timestamps
    end
  end
end
