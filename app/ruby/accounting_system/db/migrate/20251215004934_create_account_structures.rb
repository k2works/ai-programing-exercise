# frozen_string_literal: true

class CreateAccountStructures < ActiveRecord::Migration[8.1]
  def change
    create_table :account_structures, id: false do |t|
      t.string :account_code, limit: 20, null: false, primary_key: true
      t.string :account_path, limit: 200, null: false
      t.integer :hierarchy_level, null: false, default: 1
      t.string :parent_code, limit: 20
      t.integer :display_order, null: false, default: 0

      t.timestamps
    end

    # 外部キー制約
    add_foreign_key :account_structures, :accounts,
                    column: :account_code,
                    primary_key: :code,
                    on_delete: :cascade

    # パスでの検索を高速化するためのインデックス
    add_index :account_structures, :account_path,
              name: 'idx_rails_account_structure_path'

    # 親科目での検索を高速化するためのインデックス（部分インデックス）
    add_index :account_structures, :parent_code,
              where: 'parent_code IS NOT NULL',
              name: 'idx_rails_account_structure_parent'

    # 階層レベルでの検索を高速化するためのインデックス
    add_index :account_structures, :hierarchy_level,
              name: 'idx_rails_account_structure_level'

    # テーブルとカラムのコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON TABLE account_structures IS '勘定科目の階層構造を管理するマスタテーブル';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN account_structures.account_code IS '勘定科目コード';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN account_structures.account_path IS 'チルダ連結形式のパス（例: 11~11000~11190~11110）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN account_structures.hierarchy_level IS '階層の深さ（ルート=1）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN account_structures.parent_code IS '親科目のコード';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN account_structures.display_order IS '同じ階層内での表示順序';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_rails_account_structure_path IS 'パスでの検索を高速化';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_rails_account_structure_parent IS '親科目での検索を高速化（部分インデックス）';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_rails_account_structure_level IS '階層レベルでの検索を高速化';
        SQL
      end
    end
  end
end
