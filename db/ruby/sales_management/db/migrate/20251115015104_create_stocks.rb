class CreateStocks < ActiveRecord::Migration[7.1]
  def change
    create_table :stocks, id: false do |t|
      t.references :warehouse, null: false, foreign_key: true
      t.references :product, null: false, foreign_key: true
      t.string :lot_number, null: false
      t.string :stock_type, null: false
      t.string :quality_type, null: false
      t.integer :actual_quantity, default: 0, null: false
      t.integer :valid_quantity, default: 0, null: false
      t.datetime :last_shipped_at

      t.timestamps
    end

    # 複合主キーの設定
    execute <<-SQL
      ALTER TABLE stocks
      ADD CONSTRAINT stocks_pkey
      PRIMARY KEY (warehouse_id, product_id, lot_number, stock_type, quality_type);
    SQL
  end
end
