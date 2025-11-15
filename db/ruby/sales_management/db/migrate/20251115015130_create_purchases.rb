class CreatePurchases < ActiveRecord::Migration[7.1]
  def change
    create_table :purchases do |t|
      t.string :purchase_number
      t.date :purchase_date
      t.references :purchase_order, null: false, foreign_key: true
      t.references :party, null: false, foreign_key: true
      t.decimal :total_amount

      t.timestamps
    end
  end
end
