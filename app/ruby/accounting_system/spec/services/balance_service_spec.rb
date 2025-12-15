# frozen_string_literal: true

require 'rails_helper'

RSpec.describe BalanceService, type: :service do
  describe '.update_daily_balance' do
    let(:account) { create(:account, code: '1020', name: '普通預金') }
    let(:entry_date) { Date.new(2025, 1, 15) }

    before do
      DailyAccountBalance.delete_all
    end

    it '新規の日次残高レコードを作成できる' do
      # When: 日次残高を更新（新規作成）
      BalanceService.update_daily_balance(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 100_000.00,
        credit_amount: 0.00
      )

      # Then: レコードが作成されている
      balance = DailyAccountBalance.find_by(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0
      )

      expect(balance).not_to be_nil
      expect(balance.debit_amount).to eq(100_000.00)
      expect(balance.credit_amount).to eq(0.00)
    end

    it '既存の日次残高レコードに金額を加算できる（UPSERT）' do
      # Given: 既存の残高レコード
      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 100_000.00,
        credit_amount: 50_000.00
      )

      # When: 同じキーで追加の金額を更新
      BalanceService.update_daily_balance(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 75_000.00,
        credit_amount: 25_000.00
      )

      # Then: 既存レコードに金額が加算されている
      balance = DailyAccountBalance.find_by(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0
      )

      expect(balance.debit_amount).to eq(175_000.00)  # 100,000 + 75,000
      expect(balance.credit_amount).to eq(75_000.00)  # 50,000 + 25,000
    end

    it '異なる補助科目・部門・プロジェクトは別レコードとして管理される' do
      # When: 異なる部門で残高を更新
      BalanceService.update_daily_balance(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '001',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 100_000.00,
        credit_amount: 0.00
      )

      BalanceService.update_daily_balance(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '002',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 200_000.00,
        credit_amount: 0.00
      )

      # Then: 別レコードとして作成されている
      balances = DailyAccountBalance.where(
        entry_date: entry_date,
        account_code: account.code
      )

      expect(balances.count).to eq(2)
      expect(balances.pluck(:department_code).sort).to eq(%w[001 002])
    end
  end

  describe '.update_balance_from_journal' do
    let(:cash_account) { create(:account, code: '1010', name: '現金', account_type: :asset) }
    let(:sales_account) { create(:account, :revenue, code: '4010', name: '売上高') }

    before do
      DailyAccountBalance.delete_all
    end

    it '仕訳登録と同時に日次残高を更新できる' do
      # Given: 売上仕訳（現金 / 売上高）
      journal = Journal.create!(
        journal_no: 'J2025-001',
        journal_date: Date.new(2025, 1, 15),
        input_date: Date.new(2025, 1, 15),
        settlement_flag: 0
      )

      detail = journal.details.create!(
        line_number: 1,
        description: '現金売上'
      )

      # 借方：現金
      detail.items.create!(
        debit_credit_type: 'D',
        account_code: cash_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        currency_code: 'JPY',
        exchange_rate: 1.0,
        amount: 100_000.00,
        base_amount: 100_000.00,
        cash_flow_flag: 0
      )

      # 貸方：売上高
      detail.items.create!(
        debit_credit_type: 'C',
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        currency_code: 'JPY',
        exchange_rate: 1.0,
        amount: 100_000.00,
        base_amount: 100_000.00,
        cash_flow_flag: 0
      )

      # When: 仕訳から残高を更新
      BalanceService.update_balance_from_journal(journal)

      # Then: 各勘定科目の日次残高が作成されている
      cash_balance = DailyAccountBalance.find_by(
        entry_date: journal.journal_date,
        account_code: cash_account.code,
        settlement_flag: 0
      )

      sales_balance = DailyAccountBalance.find_by(
        entry_date: journal.journal_date,
        account_code: sales_account.code,
        settlement_flag: 0
      )

      expect(cash_balance).not_to be_nil
      expect(cash_balance.debit_amount).to eq(100_000.00)
      expect(cash_balance.credit_amount).to eq(0.00)

      expect(sales_balance).not_to be_nil
      expect(sales_balance.debit_amount).to eq(0.00)
      expect(sales_balance.credit_amount).to eq(100_000.00)
    end

    it '複数仕訳の登録で残高が累積される' do
      # Given: 1日目の売上仕訳
      journal1 = Journal.create!(
        journal_no: 'J2025-001',
        journal_date: Date.new(2025, 1, 15),
        input_date: Date.new(2025, 1, 15),
        settlement_flag: 0
      )

      detail1 = journal1.details.create!(line_number: 1, description: '売上1')
      detail1.items.create!(
        debit_credit_type: 'D',
        account_code: cash_account.code,
        currency_code: 'JPY',
        exchange_rate: 1.0,
        amount: 100_000.00,
        base_amount: 100_000.00,
        cash_flow_flag: 0
      )
      detail1.items.create!(
        debit_credit_type: 'C',
        account_code: sales_account.code,
        currency_code: 'JPY',
        exchange_rate: 1.0,
        amount: 100_000.00,
        base_amount: 100_000.00,
        cash_flow_flag: 0
      )

      BalanceService.update_balance_from_journal(journal1)

      # When: 同日の2件目の売上仕訳
      journal2 = Journal.create!(
        journal_no: 'J2025-002',
        journal_date: Date.new(2025, 1, 15),
        input_date: Date.new(2025, 1, 15),
        settlement_flag: 0
      )

      detail2 = journal2.details.create!(line_number: 1, description: '売上2')
      detail2.items.create!(
        debit_credit_type: 'D',
        account_code: cash_account.code,
        currency_code: 'JPY',
        exchange_rate: 1.0,
        amount: 50_000.00,
        base_amount: 50_000.00,
        cash_flow_flag: 0
      )
      detail2.items.create!(
        debit_credit_type: 'C',
        account_code: sales_account.code,
        currency_code: 'JPY',
        exchange_rate: 1.0,
        amount: 50_000.00,
        base_amount: 50_000.00,
        cash_flow_flag: 0
      )

      BalanceService.update_balance_from_journal(journal2)

      # Then: 残高が累積されている
      cash_balance = DailyAccountBalance.find_by(
        entry_date: Date.new(2025, 1, 15),
        account_code: cash_account.code,
        settlement_flag: 0
      )

      expect(cash_balance.debit_amount).to eq(150_000.00)  # 100,000 + 50,000
    end
  end
end
