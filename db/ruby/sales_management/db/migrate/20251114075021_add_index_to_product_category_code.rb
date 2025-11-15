class AddIndexToProductCategoryCode < ActiveRecord::Migration[7.1]
  def change
    add_index :product_categories, :code, unique: true
  end
end
