# frozen_string_literal: true

class CreateInvoices < ActiveRecord::Migration[7.1]
  def change
    create_table :invoices do |t|
      t.string :invoice_number
      t.date :invoice_date
      t.date :closing_date
      t.date :due_date
      t.references :party, null: false, foreign_key: true

      t.timestamps
    end

    add_index :invoices, :invoice_number, unique: true
  end
end
