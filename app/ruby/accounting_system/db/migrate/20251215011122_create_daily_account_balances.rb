# frozen_string_literal: true

class CreateDailyAccountBalances < ActiveRecord::Migration[8.1]
  def up
    create_table :daily_account_balances, id: false do |t|
      t.date :entry_date, null: false
      t.string :account_code, limit: 10, null: false
      t.string :sub_account_code, limit: 10, null: false, default: ''
      t.string :department_code, limit: 5, null: false, default: ''
      t.string :project_code, limit: 10, null: false, default: ''
      t.integer :settlement_flag, null: false, default: 0
      t.decimal :debit_amount, precision: 15, scale: 2, null: false, default: 0
      t.decimal :credit_amount, precision: 15, scale: 2, null: false, default: 0

      t.timestamps
    end

    # 複合主キー
    execute <<-SQL.squish
      ALTER TABLE daily_account_balances
        ADD PRIMARY KEY (
          entry_date,
          account_code,
          sub_account_code,
          department_code,
          project_code,
          settlement_flag
        );
    SQL

    # 外部キー制約
    add_foreign_key :daily_account_balances, :accounts,
                    column: :account_code, primary_key: :code

    # インデックス
    add_index :daily_account_balances, :entry_date, name: 'idx_daily_balance_entry_date'
    add_index :daily_account_balances, :account_code, name: 'idx_daily_balance_account'
    add_index :daily_account_balances, :department_code, name: 'idx_daily_balance_department'
    add_index :daily_account_balances, :project_code, name: 'idx_daily_balance_project'

    # CHECK制約
    execute <<-SQL.squish
      ALTER TABLE daily_account_balances
        ADD CONSTRAINT check_daily_balance_debit_amount
        CHECK (debit_amount >= 0);
    SQL

    execute <<-SQL.squish
      ALTER TABLE daily_account_balances
        ADD CONSTRAINT check_daily_balance_credit_amount
        CHECK (credit_amount >= 0);
    SQL

    execute <<-SQL.squish
      ALTER TABLE daily_account_balances
        ADD CONSTRAINT check_daily_balance_settlement_flag
        CHECK (settlement_flag IN (0, 1));
    SQL

    # テーブルコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON TABLE daily_account_balances IS '日次勘定科目残高テーブル';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.entry_date IS '実際の取引発生日';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.account_code IS '勘定科目マスタの外部キー';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.sub_account_code IS '補助科目（得意先、仕入先など）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.department_code IS '部門別管理用';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.project_code IS 'プロジェクト別管理用';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.settlement_flag IS '0=通常仕訳、1=決算仕訳';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.debit_amount IS '借方合計金額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN daily_account_balances.credit_amount IS '貸方合計金額';
        SQL
      end
    end
  end

  def down
    drop_table :daily_account_balances
  end
end
