# frozen_string_literal: true

class CreateDoubleEntryCheckFunction < ActiveRecord::Migration[8.1]
  def up
    execute <<-SQL.squish
      CREATE OR REPLACE FUNCTION check_double_entry_bookkeeping()
      RETURNS TABLE(
        inconsistent_journal_id BIGINT,
        difference DECIMAL
      ) AS $$
      BEGIN
        RETURN QUERY
        SELECT journal_id, (debit_total - credit_total) as difference
        FROM journal_balance_checks
        WHERE debit_total != credit_total;
      END;
      $$ LANGUAGE plpgsql;
    SQL
  end

  def down
    execute 'DROP FUNCTION IF EXISTS check_double_entry_bookkeeping()'
  end
end
