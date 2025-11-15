# frozen_string_literal: true

class CreateBills < ActiveRecord::Migration[7.1]
  def change
    create_table :bills do |t|
      t.string :bill_number
      t.date :bill_date
      t.date :closing_date
      t.date :due_date
      t.references :party, null: false, foreign_key: true

      t.timestamps
    end

    add_index :bills, :bill_number, unique: true
  end
end
