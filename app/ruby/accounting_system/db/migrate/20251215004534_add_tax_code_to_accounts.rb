# frozen_string_literal: true

class AddTaxCodeToAccounts < ActiveRecord::Migration[8.1]
  def change
    # 課税取引コードを追加
    add_column :accounts, :tax_code, :string, limit: 2

    # インデックスを追加（部分インデックス）
    add_index :accounts, :tax_code,
              where: 'tax_code IS NOT NULL',
              name: 'idx_accounts_on_tax_code'

    # カラムコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON COLUMN accounts.tax_code IS '課税取引コード（課税取引マスタへの外部キー）';
        SQL
        execute <<-SQL.squish
          COMMENT ON INDEX idx_accounts_on_tax_code IS '課税取引コードでの検索を高速化';
        SQL
      end
    end
  end
end
