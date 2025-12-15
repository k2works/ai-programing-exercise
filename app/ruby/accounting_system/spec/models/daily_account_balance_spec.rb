# frozen_string_literal: true

require 'rails_helper'

RSpec.describe DailyAccountBalance, type: :model do
  describe '日次勘定科目残高の基本機能' do
    let(:account) { create(:account, code: '1020', name: '普通預金') }

    before do
      DailyAccountBalance.delete_all if defined?(DailyAccountBalance)
    end

    it '日次残高レコードを登録できる' do
      # Given: 2025-01-15 の普通預金の日次残高
      entry_date = Date.new(2025, 1, 15)

      # When: 日次残高を登録
      balance = DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 100_000.00,
        credit_amount: 0.00
      )

      # Then: データが正しく登録されている
      result = DailyAccountBalance.where(
        entry_date: entry_date,
        account_code: account.code
      ).first

      expect(result).not_to be_nil
      expect(result.debit_amount).to eq(100_000.00)
      expect(result.credit_amount).to eq(0.00)
    end

    it '複合主キーで一意性が保たれる' do
      # Given: 同じキーで日次残高を登録
      entry_date = Date.new(2025, 1, 15)

      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 100_000.00,
        credit_amount: 0.00
      )

      # When & Then: 同じキーで2回目の登録を試みるとエラー
      expect do
        DailyAccountBalance.create!(
          entry_date: entry_date,
          account_code: account.code,
          sub_account_code: '',
          department_code: '',
          project_code: '',
          settlement_flag: 0,
          debit_amount: 50_000.00,
          credit_amount: 0.00
        )
      end.to raise_error(ActiveRecord::RecordNotUnique)
    end

    it '部門別の残高を管理できる' do
      # Given: 売上高の部門別日次残高
      entry_date = Date.new(2025, 1, 15)
      sales_account = create(:account, :revenue, code: '4010', name: '売上高')

      # When: 部門001と部門002の残高を登録
      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '001',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 0.00,
        credit_amount: 300_000.00
      )

      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '002',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 0.00,
        credit_amount: 200_000.00
      )

      # Then: 部門別に集計できる
      result = DailyAccountBalance.where(account_code: sales_account.code)
                                   .group(:department_code)
                                   .select('department_code, SUM(credit_amount) as sales_total')
                                   .order(:department_code)
                                   .to_a

      expect(result.length).to eq(2)
      expect(result[0].department_code).to eq('001')
      expect(result[0].sales_total.to_f).to eq(300_000.00)
      expect(result[1].department_code).to eq('002')
      expect(result[1].sales_total.to_f).to eq(200_000.00)
    end

    it 'プロジェクト別の残高を管理できる' do
      # Given: プロジェクト別の残高
      entry_date = Date.new(2025, 1, 15)
      sales_account = create(:account, :revenue, code: '4010', name: '売上高')

      # When: プロジェクトP001とP002の残高を登録
      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: 'P001',
        settlement_flag: 0,
        debit_amount: 0.00,
        credit_amount: 150_000.00
      )

      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: 'P002',
        settlement_flag: 0,
        debit_amount: 0.00,
        credit_amount: 250_000.00
      )

      # Then: プロジェクト別に集計できる
      result = DailyAccountBalance.where(account_code: sales_account.code)
                                   .group(:project_code)
                                   .select('project_code, SUM(credit_amount) as sales_total')
                                   .order(:project_code)
                                   .to_a

      expect(result.length).to eq(2)
      expect(result[0].project_code).to eq('P001')
      expect(result[0].sales_total.to_f).to eq(150_000.00)
      expect(result[1].project_code).to eq('P002')
      expect(result[1].sales_total.to_f).to eq(250_000.00)
    end

    it '補助科目別の残高を管理できる' do
      # Given: 売掛金の補助科目（得意先）別残高
      entry_date = Date.new(2025, 1, 15)
      receivable_account = create(:account, code: '1130', name: '売掛金', account_type: :asset)

      # When: 得意先A001とA002の残高を登録
      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: receivable_account.code,
        sub_account_code: 'A001',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 500_000.00,
        credit_amount: 0.00
      )

      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: receivable_account.code,
        sub_account_code: 'A002',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 300_000.00,
        credit_amount: 0.00
      )

      # Then: 補助科目別に集計できる
      result = DailyAccountBalance.where(account_code: receivable_account.code)
                                   .group(:sub_account_code)
                                   .select('sub_account_code, SUM(debit_amount) as receivable_total')
                                   .order(:sub_account_code)
                                   .to_a

      expect(result.length).to eq(2)
      expect(result[0].sub_account_code).to eq('A001')
      expect(result[0].receivable_total.to_f).to eq(500_000.00)
      expect(result[1].sub_account_code).to eq('A002')
      expect(result[1].receivable_total.to_f).to eq(300_000.00)
    end

    it '決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる' do
      # Given: 通常仕訳と決算仕訳の残高
      entry_date = Date.new(2025, 3, 31)
      cost_account = create(:account, :expense, code: '5110', name: '仕入')

      # When: 通常仕訳と決算仕訳の残高を登録
      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: cost_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        debit_amount: 1_000_000.00,
        credit_amount: 0.00
      )

      DailyAccountBalance.create!(
        entry_date: entry_date,
        account_code: cost_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 1,
        debit_amount: 50_000.00,
        credit_amount: 0.00
      )

      # Then: 決算仕訳フラグで区別できる
      # 通常仕訳のみの合計
      normal_total = DailyAccountBalance.where(
        account_code: cost_account.code,
        settlement_flag: 0
      ).sum(:debit_amount)

      # 決算仕訳のみの合計
      settlement_total = DailyAccountBalance.where(
        account_code: cost_account.code,
        settlement_flag: 1
      ).sum(:debit_amount)

      expect(normal_total.to_f).to eq(1_000_000.00)
      expect(settlement_total.to_f).to eq(50_000.00)
    end
  end
end
