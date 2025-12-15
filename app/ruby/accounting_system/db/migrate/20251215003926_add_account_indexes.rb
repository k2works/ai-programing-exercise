# frozen_string_literal: true

class AddAccountIndexes < ActiveRecord::Migration[8.1]
  def change
    # BSPL区分のインデックス（部分インデックス）
    add_index :accounts, :bspl_type,
              where: 'bspl_type IS NOT NULL',
              name: 'idx_accounts_on_bspl_type'

    # 取引要素区分のインデックス（部分インデックス）
    add_index :accounts, :transaction_type,
              where: 'transaction_type IS NOT NULL',
              name: 'idx_accounts_on_transaction_type'

    # 表示順序のインデックス（部分インデックス）
    add_index :accounts, :display_order,
              where: 'display_order IS NOT NULL',
              name: 'idx_accounts_on_display_order'

    # 複合インデックス（BSPL区分 + 表示順序）
    add_index :accounts, %i[bspl_type display_order],
              where: 'bspl_type IS NOT NULL AND display_order IS NOT NULL',
              name: 'idx_accounts_on_bspl_type_and_display_order'

    # インデックスコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON INDEX idx_accounts_on_bspl_type IS 'BSPL区分での検索を高速化';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_accounts_on_transaction_type IS '取引要素区分での検索を高速化';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_accounts_on_display_order IS '表示順序でのソートを高速化';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_accounts_on_bspl_type_and_display_order IS 'BSPL区分での絞り込み + 表示順序でのソートを高速化（財務諸表表示用）';
        SQL
      end
    end
  end
end
