# frozen_string_literal: true

class AddAdditionalFieldsToAccounts < ActiveRecord::Migration[8.1]
  def change
    change_table :accounts, bulk: true do |t|
      # 取引要素区分を追加
      t.string :transaction_type, limit: 1

      # 費用区分を追加
      t.string :expense_type, limit: 1

      # 表示順序を追加
      t.integer :display_order
    end

    # カラムコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.transaction_type IS '取引要素区分（1:資産, 2:負債, 3:純資産, 4:収益, 5:費用）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.expense_type IS '費用区分（1:販管費, 2:営業外費用, 3:特別損失）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.display_order IS '表示順序';
        SQL
      end
    end
  end
end
