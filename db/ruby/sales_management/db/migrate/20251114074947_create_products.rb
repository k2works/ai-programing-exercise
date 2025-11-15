class CreateProducts < ActiveRecord::Migration[7.1]
  def change
    create_table :products do |t|
      t.string :code
      t.string :name
      t.decimal :unit_price
      t.references :product_category, null: false, foreign_key: true

      t.timestamps
    end
  end
end
