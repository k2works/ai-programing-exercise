class CreateWarehouses < ActiveRecord::Migration[7.1]
  def change
    create_table :warehouses do |t|
      t.string :code
      t.string :name
      t.integer :warehouse_type
      t.string :address
      t.string :phone

      t.timestamps
    end
  end
end
