class CreateEmployees < ActiveRecord::Migration[7.1]
  def change
    create_table :employees do |t|
      t.string :code
      t.string :first_name
      t.string :last_name
      t.references :department, null: false, foreign_key: true

      t.timestamps
    end
  end
end
