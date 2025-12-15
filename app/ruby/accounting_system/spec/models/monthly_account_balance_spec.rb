# frozen_string_literal: true

require 'rails_helper'

RSpec.describe MonthlyAccountBalance, type: :model do
  describe '月次勘定科目残高の基本機能' do
    let(:account) { create(:account, code: '1020', name: '普通預金') }

    before do
      MonthlyAccountBalance.delete_all if defined?(MonthlyAccountBalance)
    end

    it '月次残高レコードを登録できる' do
      # Given: 2025年1月の普通預金の月次残高
      fiscal_year = 2025
      month = 1

      # When: 月次残高を登録
      balance = MonthlyAccountBalance.create!(
        fiscal_year: fiscal_year,
        month: month,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 1_000_000.00,
        debit_amount: 500_000.00,
        credit_amount: 200_000.00,
        ending_balance: 1_300_000.00
      )

      # Then: データが正しく登録されている
      result = MonthlyAccountBalance.where(
        fiscal_year: fiscal_year,
        month: month,
        account_code: account.code
      ).first

      expect(result).not_to be_nil
      expect(result.beginning_balance).to eq(1_000_000.00)
      expect(result.debit_amount).to eq(500_000.00)
      expect(result.credit_amount).to eq(200_000.00)
      expect(result.ending_balance).to eq(1_300_000.00)
    end

    it '複合主キーで一意性が保たれる' do
      # Given: 同じキーで月次残高を登録
      fiscal_year = 2025
      month = 1

      MonthlyAccountBalance.create!(
        fiscal_year: fiscal_year,
        month: month,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 1_000_000.00,
        debit_amount: 500_000.00,
        credit_amount: 200_000.00,
        ending_balance: 1_300_000.00
      )

      # When & Then: 同じキーで2回目の登録を試みるとエラー
      expect do
        MonthlyAccountBalance.create!(
          fiscal_year: fiscal_year,
          month: month,
          account_code: account.code,
          sub_account_code: '',
          department_code: '',
          project_code: '',
          settlement_flag: 0,
          beginning_balance: 2_000_000.00,
          debit_amount: 300_000.00,
          credit_amount: 100_000.00,
          ending_balance: 2_200_000.00
        )
      end.to raise_error(ActiveRecord::RecordNotUnique)
    end

    it '月度は1〜12の範囲でバリデーションされる' do
      # Given: 無効な月度（13月）
      balance = MonthlyAccountBalance.new(
        fiscal_year: 2025,
        month: 13,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 0,
        debit_amount: 0,
        credit_amount: 0,
        ending_balance: 0
      )

      # When & Then: バリデーションエラー
      expect(balance).not_to be_valid
      expect(balance.errors[:month]).to include('is not included in the list')
    end

    it '月末残高を計算できる' do
      # Given: 月次残高データ
      balance = MonthlyAccountBalance.create!(
        fiscal_year: 2025,
        month: 1,
        account_code: account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 1_000_000.00,
        debit_amount: 500_000.00,
        credit_amount: 200_000.00,
        ending_balance: 0
      )

      # When: 月末残高を計算
      calculated_ending = balance.calculate_ending_balance

      # Then: 月初残高 + 借方金額 - 貸方金額 = 月末残高
      expect(calculated_ending).to eq(1_300_000.00)
    end

    it '会計年度別に集計できる' do
      # Given: 2024年と2025年の売上高残高
      sales_account = create(:account, :revenue, code: '4010', name: '売上高')

      MonthlyAccountBalance.create!(
        fiscal_year: 2024,
        month: 12,
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 0,
        debit_amount: 0,
        credit_amount: 10_000_000.00,
        ending_balance: 10_000_000.00
      )

      MonthlyAccountBalance.create!(
        fiscal_year: 2025,
        month: 1,
        account_code: sales_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 0,
        debit_amount: 0,
        credit_amount: 5_000_000.00,
        ending_balance: 5_000_000.00
      )

      # When: 2025年度のみを取得
      result_2025 = MonthlyAccountBalance.by_fiscal_year(2025)
                                         .where(account_code: sales_account.code)

      # Then: 2025年度のデータのみ取得できる
      expect(result_2025.count).to eq(1)
      expect(result_2025.first.fiscal_year).to eq(2025)
      expect(result_2025.first.credit_amount.to_f).to eq(5_000_000.00)
    end

    it '決算仕訳フラグで通常仕訳と決算仕訳を分けて管理できる' do
      # Given: 通常仕訳と決算仕訳の月次残高
      cost_account = create(:account, :expense, code: '5110', name: '仕入')

      # 通常仕訳
      MonthlyAccountBalance.create!(
        fiscal_year: 2025,
        month: 3,
        account_code: cost_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 0,
        beginning_balance: 0,
        debit_amount: 8_000_000.00,
        credit_amount: 0,
        ending_balance: 8_000_000.00
      )

      # 決算仕訳
      MonthlyAccountBalance.create!(
        fiscal_year: 2025,
        month: 3,
        account_code: cost_account.code,
        sub_account_code: '',
        department_code: '',
        project_code: '',
        settlement_flag: 1,
        beginning_balance: 0,
        debit_amount: 500_000.00,
        credit_amount: 0,
        ending_balance: 500_000.00
      )

      # When: 通常仕訳のみを集計
      normal_total = MonthlyAccountBalance.normal_entries
                                          .where(account_code: cost_account.code)
                                          .sum(:debit_amount)

      # Then: 通常仕訳のみの合計が取得できる
      expect(normal_total.to_f).to eq(8_000_000.00)
    end
  end
end
