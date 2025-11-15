class AddIndexToEmployeeCode < ActiveRecord::Migration[7.1]
  def change
    add_index :employees, :code, unique: true
  end
end
