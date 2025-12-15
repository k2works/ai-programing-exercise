# frozen_string_literal: true

class CreateMonthlyAccountBalances < ActiveRecord::Migration[8.1]
  def up
    create_table :monthly_account_balances, id: false do |t|
      t.integer :fiscal_year, null: false
      t.integer :month, null: false
      t.string :account_code, limit: 10, null: false
      t.string :sub_account_code, limit: 10, null: false, default: ''
      t.string :department_code, limit: 5, null: false, default: ''
      t.string :project_code, limit: 10, null: false, default: ''
      t.integer :settlement_flag, null: false, default: 0
      t.decimal :beginning_balance, precision: 15, scale: 2, null: false, default: 0
      t.decimal :debit_amount, precision: 15, scale: 2, null: false, default: 0
      t.decimal :credit_amount, precision: 15, scale: 2, null: false, default: 0
      t.decimal :ending_balance, precision: 15, scale: 2, null: false, default: 0

      t.timestamps
    end

    # 複合主キー
    execute <<-SQL.squish
      ALTER TABLE monthly_account_balances
        ADD PRIMARY KEY (
          fiscal_year,
          month,
          account_code,
          sub_account_code,
          department_code,
          project_code,
          settlement_flag
        );
    SQL

    # 外部キー制約
    add_foreign_key :monthly_account_balances, :accounts,
                    column: :account_code, primary_key: :code

    # インデックス
    add_index :monthly_account_balances, %i[fiscal_year month], name: 'idx_monthly_balance_fiscal_month'
    add_index :monthly_account_balances, :account_code, name: 'idx_monthly_balance_account'
    add_index :monthly_account_balances, :department_code, name: 'idx_monthly_balance_department'
    add_index :monthly_account_balances, :project_code, name: 'idx_monthly_balance_project'

    # CHECK制約
    execute <<-SQL.squish
      ALTER TABLE monthly_account_balances
        ADD CONSTRAINT check_monthly_balance_month_range
        CHECK (month >= 1 AND month <= 12);
    SQL

    execute <<-SQL.squish
      ALTER TABLE monthly_account_balances
        ADD CONSTRAINT check_monthly_balance_debit_amount
        CHECK (debit_amount >= 0);
    SQL

    execute <<-SQL.squish
      ALTER TABLE monthly_account_balances
        ADD CONSTRAINT check_monthly_balance_credit_amount
        CHECK (credit_amount >= 0);
    SQL

    execute <<-SQL.squish
      ALTER TABLE monthly_account_balances
        ADD CONSTRAINT check_monthly_balance_settlement_flag
        CHECK (settlement_flag IN (0, 1));
    SQL

    # テーブルコメント
    reversible do |dir|
      dir.up do
        execute <<-SQL.squish
          COMMENT ON TABLE monthly_account_balances IS '月次勘定科目残高テーブル';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.fiscal_year IS '会計年度（例：2025）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.month IS '月度（1～12）';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.account_code IS '勘定科目マスタの外部キー';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.sub_account_code IS '補助科目';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.department_code IS '部門別管理用';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.project_code IS 'プロジェクト別管理用';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.settlement_flag IS '0=通常仕訳、1=決算仕訳';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.beginning_balance IS '月初時点の残高';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.debit_amount IS '借方合計金額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.credit_amount IS '貸方合計金額';
        SQL
        execute <<-SQL.squish
          COMMENT ON COLUMN monthly_account_balances.ending_balance IS '月末時点の残高';
        SQL
      end
    end
  end

  def down
    drop_table :monthly_account_balances
  end
end
