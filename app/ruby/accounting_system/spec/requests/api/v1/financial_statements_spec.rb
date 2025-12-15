# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Api::V1::FinancialStatements', type: :request do
  # テストデータのセットアップ
  before do
    # 勘定科目の作成
    @cash = create(:account, code: '1010', name: '現金', account_type: :asset, transaction_type: '1')
    @accounts_receivable = create(:account, code: '1110', name: '売掛金', account_type: :asset, transaction_type: '1')
    @accounts_payable = create(:account, :liability, code: '2110', name: '買掛金', transaction_type: '2')
    @capital = create(:account, :equity, code: '3010', name: '資本金', transaction_type: '3')
    @sales = create(:account, :revenue, code: '4010', name: '売上高', transaction_type: '4')
    @cost_of_sales = create(:account, :expense, code: '5110', name: '売上原価', transaction_type: '5')
    @selling_expense = create(:account, :expense, code: '6010', name: '販売費', transaction_type: '5')

    # 基準日
    @as_of_date = Date.new(2025, 3, 31)
    @from_date = Date.new(2025, 1, 1)
    @to_date = Date.new(2025, 3, 31)

    # 日次残高の作成（貸借対照表用）
    # 現金: 500,000円（借方）
    DailyAccountBalance.create!(
      account_code: @cash.code,
      entry_date: @as_of_date,
      debit_amount: 500_000,
      credit_amount: 0
    )

    # 売掛金: 300,000円（借方）
    DailyAccountBalance.create!(
      account_code: @accounts_receivable.code,
      entry_date: @as_of_date,
      debit_amount: 300_000,
      credit_amount: 0
    )

    # 買掛金: 200,000円（貸方）
    DailyAccountBalance.create!(
      account_code: @accounts_payable.code,
      entry_date: @as_of_date,
      debit_amount: 0,
      credit_amount: 200_000
    )

    # 資本金: 600,000円（貸方）
    DailyAccountBalance.create!(
      account_code: @capital.code,
      entry_date: @as_of_date,
      debit_amount: 0,
      credit_amount: 600_000
    )

    # 日次残高の作成（損益計算書用）
    # 売上高: 1,000,000円（貸方）
    DailyAccountBalance.create!(
      account_code: @sales.code,
      entry_date: @to_date,
      debit_amount: 0,
      credit_amount: 1_000_000
    )

    # 売上原価: 600,000円（借方）
    DailyAccountBalance.create!(
      account_code: @cost_of_sales.code,
      entry_date: @to_date,
      debit_amount: 600_000,
      credit_amount: 0
    )

    # 販売費: 200,000円（借方）
    DailyAccountBalance.create!(
      account_code: @selling_expense.code,
      entry_date: @to_date,
      debit_amount: 200_000,
      credit_amount: 0
    )
  end

  describe 'GET /api/v1/financial-statements/balance-sheet' do
    context '正しいパラメータの場合' do
      it '貸借対照表を取得できる' do
        # When: 貸借対照表を取得
        get '/api/v1/financial-statements/balance-sheet', params: { as_of_date: @as_of_date.to_s }

        # Then: 正常なレスポンスが返る
        expect(response).to have_http_status(:ok)
        expect(response.content_type).to include('application/json')

        json = JSON.parse(response.body)
        expect(json['as_of_date']).to eq(@as_of_date.to_s)
        expect(json['assets']).to be_a(Array)
        expect(json['liabilities']).to be_a(Array)
        expect(json['equity']).to be_a(Array)

        # 資産合計 = 800,000円（現金 500,000 + 売掛金 300,000）
        expect(json['total_assets']).to eq('800000.0')

        # 負債合計 = 200,000円
        expect(json['total_liabilities']).to eq('200000.0')

        # 純資産合計 = 600,000円
        expect(json['total_equity']).to eq('600000.0')

        # 貸借平衡の原則
        expect(json['total_liabilities_and_equity']).to eq(json['total_assets'])
      end
    end

    context 'パラメータが不正な場合' do
      it '日付パラメータがない場合はエラーを返す' do
        # When: 日付パラメータなしで取得
        get '/api/v1/financial-statements/balance-sheet'

        # Then: 400 Bad Requestが返る
        expect(response).to have_http_status(:bad_request)

        json = JSON.parse(response.body)
        expect(json['error']).to include('日付パラメータが必要です')
      end

      it '無効な日付形式の場合はエラーを返す' do
        # When: 無効な日付形式で取得
        get '/api/v1/financial-statements/balance-sheet', params: { as_of_date: 'invalid-date' }

        # Then: 400 Bad Requestが返る
        expect(response).to have_http_status(:bad_request)

        json = JSON.parse(response.body)
        expect(json['error']).to include('無効な日付形式です')
      end
    end
  end

  describe 'GET /api/v1/financial-statements/income-statement' do
    context '正しいパラメータの場合' do
      it '損益計算書を取得できる' do
        # When: 損益計算書を取得
        get '/api/v1/financial-statements/income-statement',
            params: { from_date: @from_date.to_s, to_date: @to_date.to_s }

        # Then: 正常なレスポンスが返る
        expect(response).to have_http_status(:ok)
        expect(response.content_type).to include('application/json')

        json = JSON.parse(response.body)
        expect(json['from_date']).to eq(@from_date.to_s)
        expect(json['to_date']).to eq(@to_date.to_s)
        expect(json['revenues']).to be_a(Array)
        expect(json['expenses']).to be_a(Array)

        # 売上高 = 1,000,000円
        expect(json['total_revenues']).to eq('1000000.0')

        # 費用合計 = 800,000円（売上原価 600,000 + 販売費 200,000）
        expect(json['total_expenses']).to eq('800000.0')

        # 売上総利益 = 400,000円（売上高 1,000,000 - 売上原価 600,000）
        expect(json['gross_profit']).to eq('400000.0')

        # 営業利益 = 200,000円（売上総利益 400,000 - 販売費 200,000）
        expect(json['operating_income']).to eq('200000.0')

        # 当期純利益 = 200,000円（売上高 1,000,000 - 費用合計 800,000）
        expect(json['net_income']).to eq('200000.0')
      end
    end

    context 'パラメータが不正な場合' do
      it '日付パラメータがない場合はエラーを返す' do
        # When: 日付パラメータなしで取得
        get '/api/v1/financial-statements/income-statement'

        # Then: 400 Bad Requestが返る
        expect(response).to have_http_status(:bad_request)

        json = JSON.parse(response.body)
        expect(json['error']).to include('日付パラメータが必要です')
      end

      it '開始日が終了日より後の場合はエラーを返す' do
        # When: 開始日 > 終了日で取得
        get '/api/v1/financial-statements/income-statement',
            params: { from_date: @to_date.to_s, to_date: @from_date.to_s }

        # Then: 400 Bad Requestが返る
        expect(response).to have_http_status(:bad_request)

        json = JSON.parse(response.body)
        expect(json['error']).to include('開始日は終了日以前である必要があります')
      end
    end
  end

  describe 'GET /api/v1/financial-statements/ratios' do
    context '正しいパラメータの場合' do
      it '財務指標を取得できる' do
        # When: 財務指標を取得
        get '/api/v1/financial-statements/ratios',
            params: {
              as_of_date: @as_of_date.to_s,
              from_date: @from_date.to_s,
              to_date: @to_date.to_s
            }

        # Then: 正常なレスポンスが返る
        expect(response).to have_http_status(:ok)
        expect(response.content_type).to include('application/json')

        json = JSON.parse(response.body)

        # 流動比率が計算されている
        expect(json).to have_key('current_ratio')
        expect(json['current_ratio']).not_to be_nil

        # 自己資本比率が計算されている
        expect(json).to have_key('debt_to_equity_ratio')
        expect(json['debt_to_equity_ratio'].to_f).to eq(75.0) # (600,000 / 800,000) * 100

        # 利益率が計算されている
        expect(json).to have_key('gross_profit_margin')
        expect(json['gross_profit_margin'].to_f).to eq(40.0) # (400,000 / 1,000,000) * 100

        expect(json).to have_key('operating_profit_margin')
        expect(json['operating_profit_margin'].to_f).to eq(20.0) # (200,000 / 1,000,000) * 100

        expect(json).to have_key('net_profit_margin')
        expect(json['net_profit_margin'].to_f).to eq(20.0) # (200,000 / 1,000,000) * 100

        # ROA, ROEが計算されている
        expect(json).to have_key('roa')
        expect(json['roa'].to_f).to eq(25.0) # (200,000 / 800,000) * 100

        expect(json).to have_key('roe')
        expect(json['roe'].to_f).to be > 0
      end
    end

    context 'パラメータが不正な場合' do
      it '日付パラメータがない場合はエラーを返す' do
        # When: 日付パラメータなしで取得
        get '/api/v1/financial-statements/ratios'

        # Then: 400 Bad Requestが返る
        expect(response).to have_http_status(:bad_request)

        json = JSON.parse(response.body)
        expect(json['error']).to include('日付パラメータが必要です')
      end
    end
  end
end
