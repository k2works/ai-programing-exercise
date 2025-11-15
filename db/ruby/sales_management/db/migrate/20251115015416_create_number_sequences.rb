# frozen_string_literal: true

class CreateNumberSequences < ActiveRecord::Migration[7.1]
  def change
    create_table :number_sequences do |t|
      t.string :sequence_type
      t.string :prefix
      t.integer :current_number
      t.date :last_generated_date

      t.timestamps
    end

    add_index :number_sequences, :sequence_type, unique: true
  end
end
