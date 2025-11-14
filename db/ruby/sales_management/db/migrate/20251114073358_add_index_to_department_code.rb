class AddIndexToDepartmentCode < ActiveRecord::Migration[7.1]
  def change
    add_index :departments, :code, unique: true
  end
end
