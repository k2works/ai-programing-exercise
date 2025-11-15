class AddIndexToWarehouseCode < ActiveRecord::Migration[7.1]
  def change
    add_index :warehouses, :code, unique: true
  end
end
