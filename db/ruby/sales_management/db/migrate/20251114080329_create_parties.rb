class CreateParties < ActiveRecord::Migration[7.1]
  def change
    create_table :parties do |t|
      t.string :party_type

      t.timestamps
    end
  end
end
