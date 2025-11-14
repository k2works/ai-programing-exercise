class AddIndexToOrderNumber < ActiveRecord::Migration[7.1]
  def change
    add_index :orders, :order_number, unique: true
  end
end
