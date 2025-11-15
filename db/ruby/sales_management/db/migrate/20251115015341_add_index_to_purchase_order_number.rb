class AddIndexToPurchaseOrderNumber < ActiveRecord::Migration[7.1]
  def change
    add_index :purchase_orders, :order_number, unique: true
  end
end
