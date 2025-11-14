class CreateOrders < ActiveRecord::Migration[7.1]
  def change
    create_table :orders do |t|
      t.string :order_number
      t.string :order_type
      t.date :order_date
      t.string :status
      t.references :party, null: false, foreign_key: true

      t.timestamps
    end
  end
end
