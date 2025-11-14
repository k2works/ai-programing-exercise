class CreateOrganizations < ActiveRecord::Migration[7.1]
  def change
    create_table :organizations do |t|
      t.references :party, null: false, foreign_key: true
      t.string :name
      t.string :tax_id

      t.timestamps
    end
  end
end
