class AddIndexToPurchaseNumber < ActiveRecord::Migration[7.1]
  def change
    add_index :purchases, :purchase_number, unique: true
  end
end
