class CreatePurchaseItems < ActiveRecord::Migration[7.1]
  def change
    create_table :purchase_items do |t|
      t.references :purchase, null: false, foreign_key: true
      t.references :product, null: false, foreign_key: true
      t.string :lot_number
      t.references :warehouse, null: false, foreign_key: true
      t.integer :quantity
      t.decimal :unit_price

      t.timestamps
    end
  end
end
