# frozen_string_literal: true

class CreateJournalBalanceCheckView < ActiveRecord::Migration[8.1]
  def up
    execute <<-SQL.squish
      CREATE OR REPLACE VIEW journal_balance_checks AS
      SELECT
        jd.journal_id,
        SUM(CASE WHEN jdi.debit_credit_type = 'D' THEN jdi.amount ELSE 0 END) AS debit_total,
        SUM(CASE WHEN jdi.debit_credit_type = 'C' THEN jdi.amount ELSE 0 END) AS credit_total,
        SUM(CASE WHEN jdi.debit_credit_type = 'D' THEN jdi.amount ELSE 0 END) -
        SUM(CASE WHEN jdi.debit_credit_type = 'C' THEN jdi.amount ELSE 0 END) AS difference
      FROM journal_details jd
      INNER JOIN journal_detail_items jdi ON jd.id = jdi.journal_detail_id
      GROUP BY jd.journal_id
    SQL
  end

  def down
    execute 'DROP VIEW IF EXISTS journal_balance_checks'
  end
end
