class CreatePurchaseOrders < ActiveRecord::Migration[7.1]
  def change
    create_table :purchase_orders do |t|
      t.string :order_number
      t.date :order_date
      t.references :party, null: false, foreign_key: true
      t.references :warehouse, null: false, foreign_key: true
      t.references :sales_order, foreign_key: { to_table: :orders }, null: true
      t.string :status
      t.decimal :total_amount

      t.timestamps
    end
  end
end
