class CreatePartyRoles < ActiveRecord::Migration[7.1]
  def change
    create_table :party_roles do |t|
      t.references :party, null: false, foreign_key: true
      t.string :role_type
      t.datetime :started_at
      t.datetime :ended_at

      t.timestamps
    end
  end
end
