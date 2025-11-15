class AddIndexToProductCode < ActiveRecord::Migration[7.1]
  def change
    add_index :products, :code, unique: true
  end
end
