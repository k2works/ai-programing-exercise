# frozen_string_literal: true

class AddAccountConstraints < ActiveRecord::Migration[8.1]
  def up
    # BSPL区分のCHECK制約
    execute <<-SQL.squish
      ALTER TABLE accounts
        ADD CONSTRAINT check_bspl_type
        CHECK (bspl_type IN ('B', 'P') OR bspl_type IS NULL);
    SQL

    # 取引要素区分のCHECK制約
    execute <<-SQL.squish
      ALTER TABLE accounts
        ADD CONSTRAINT check_transaction_type
        CHECK (transaction_type IN ('1', '2', '3', '4', '5') OR transaction_type IS NULL);
    SQL

    # 費用区分のCHECK制約
    execute <<-SQL.squish
      ALTER TABLE accounts
        ADD CONSTRAINT check_expense_type
        CHECK (expense_type IN ('1', '2', '3') OR expense_type IS NULL);
    SQL

    # BSPL区分と勘定科目種別の整合性チェック
    execute <<-SQL.squish
      ALTER TABLE accounts
        ADD CONSTRAINT check_bspl_consistency
        CHECK (
          (bspl_type = 'B' AND account_type IN (0, 1, 2))
          OR
          (bspl_type = 'P' AND account_type IN (3, 4))
          OR
          (bspl_type IS NULL)
        );
    SQL

    # 費用区分は費用科目のみ
    execute <<-SQL.squish
      ALTER TABLE accounts
        ADD CONSTRAINT check_expense_type_only_for_expense
        CHECK (
          (expense_type IS NOT NULL AND account_type = 4)
          OR
          (expense_type IS NULL)
        );
    SQL
  end

  def down
    execute 'ALTER TABLE accounts DROP CONSTRAINT IF EXISTS check_bspl_type;'
    execute 'ALTER TABLE accounts DROP CONSTRAINT IF EXISTS check_transaction_type;'
    execute 'ALTER TABLE accounts DROP CONSTRAINT IF EXISTS check_expense_type;'
    execute 'ALTER TABLE accounts DROP CONSTRAINT IF EXISTS check_bspl_consistency;'
    execute 'ALTER TABLE accounts DROP CONSTRAINT IF EXISTS check_expense_type_only_for_expense;'
  end
end
