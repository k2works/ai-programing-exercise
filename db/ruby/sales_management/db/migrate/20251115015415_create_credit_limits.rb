# frozen_string_literal: true

class CreateCreditLimits < ActiveRecord::Migration[7.1]
  def change
    create_table :credit_limits do |t|
      t.references :party, null: false, foreign_key: true
      t.decimal :limit_amount
      t.decimal :used_amount

      t.timestamps
    end
  end
end
