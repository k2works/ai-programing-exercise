# frozen_string_literal: true

require 'swagger_helper'

RSpec.describe 'api/v1/financial_statements', type: :request do
  let!(:cash) { create(:account, code: '1010', name: '現金', account_type: :asset, transaction_type: '1') }
  let!(:capital) { create(:account, :equity, code: '3010', name: '資本金', transaction_type: '3') }
  let!(:sales) { create(:account, :revenue, code: '4010', name: '売上高', transaction_type: '4') }
  let!(:cost_of_sales) { create(:account, :expense, code: '5110', name: '売上原価', transaction_type: '5') }

  before do
    # テストデータの作成
    DailyAccountBalance.create!(
      account_code: cash.code,
      entry_date: Date.new(2025, 3, 31),
      debit_amount: 500_000,
      credit_amount: 0
    )
    DailyAccountBalance.create!(
      account_code: capital.code,
      entry_date: Date.new(2025, 3, 31),
      debit_amount: 0,
      credit_amount: 500_000
    )
    DailyAccountBalance.create!(
      account_code: sales.code,
      entry_date: Date.new(2025, 3, 31),
      debit_amount: 0,
      credit_amount: 1_000_000
    )
    DailyAccountBalance.create!(
      account_code: cost_of_sales.code,
      entry_date: Date.new(2025, 3, 31),
      debit_amount: 600_000,
      credit_amount: 0
    )
  end

  path '/api/v1/financial-statements/balance-sheet' do
    get('貸借対照表取得') do
      tags '財務諸表'
      description '指定日時点の貸借対照表を取得します'
      produces 'application/json'

      parameter name: :as_of_date, in: :query, type: :string, format: :date, description: '基準日（YYYY-MM-DD）', required: true

      response(200, 'successful') do
        let(:as_of_date) { '2025-03-31' }

        schema type: :object,
               properties: {
                 as_of_date: { type: :string, format: :date, description: '基準日' },
                 assets: {
                   type: :array,
                   description: '資産',
                   items: {
                     type: :object,
                     properties: {
                       account_code: { type: :string, description: '勘定科目コード' },
                       account_name: { type: :string, description: '勘定科目名' },
                       balance: { type: :number, description: '残高' },
                       percentage: { type: :number, description: '構成比率（%）' }
                     }
                   }
                 },
                 liabilities: {
                   type: :array,
                   description: '負債',
                   items: {
                     type: :object,
                     properties: {
                       account_code: { type: :string, description: '勘定科目コード' },
                       account_name: { type: :string, description: '勘定科目名' },
                       balance: { type: :number, description: '残高' },
                       percentage: { type: :number, description: '構成比率（%）' }
                     }
                   }
                 },
                 equity: {
                   type: :array,
                   description: '純資産',
                   items: {
                     type: :object,
                     properties: {
                       account_code: { type: :string, description: '勘定科目コード' },
                       account_name: { type: :string, description: '勘定科目名' },
                       balance: { type: :number, description: '残高' },
                       percentage: { type: :number, description: '構成比率（%）' }
                     }
                   }
                 },
                 total_assets: { type: :number, description: '資産合計' },
                 total_liabilities: { type: :number, description: '負債合計' },
                 total_equity: { type: :number, description: '純資産合計' },
                 total_liabilities_and_equity: { type: :number, description: '負債・純資産合計' }
               },
               required: %w[as_of_date assets liabilities equity total_assets total_liabilities total_equity total_liabilities_and_equity]

        run_test!
      end

      response(400, 'bad request') do
        let(:as_of_date) { 'invalid-date' }

        run_test!
      end
    end
  end

  path '/api/v1/financial-statements/income-statement' do
    get('損益計算書取得') do
      tags '財務諸表'
      description '指定期間の損益計算書を取得します'
      produces 'application/json'

      parameter name: :from_date, in: :query, type: :string, format: :date, description: '開始日（YYYY-MM-DD）', required: true
      parameter name: :to_date, in: :query, type: :string, format: :date, description: '終了日（YYYY-MM-DD）', required: true

      response(200, 'successful') do
        let(:from_date) { '2025-01-01' }
        let(:to_date) { '2025-03-31' }

        schema type: :object,
               properties: {
                 from_date: { type: :string, format: :date, description: '開始日' },
                 to_date: { type: :string, format: :date, description: '終了日' },
                 revenues: {
                   type: :array,
                   description: '収益',
                   items: {
                     type: :object,
                     properties: {
                       account_code: { type: :string, description: '勘定科目コード' },
                       account_name: { type: :string, description: '勘定科目名' },
                       balance: { type: :number, description: '金額' },
                       percentage: { type: :number, description: '対売上比（%）' }
                     }
                   }
                 },
                 expenses: {
                   type: :array,
                   description: '費用',
                   items: {
                     type: :object,
                     properties: {
                       account_code: { type: :string, description: '勘定科目コード' },
                       account_name: { type: :string, description: '勘定科目名' },
                       balance: { type: :number, description: '金額' },
                       percentage: { type: :number, description: '対売上比（%）' }
                     }
                   }
                 },
                 gross_profit: { type: :number, description: '売上総利益' },
                 operating_income: { type: :number, description: '営業利益' },
                 net_income: { type: :number, description: '当期純利益' },
                 total_revenues: { type: :number, description: '収益合計' },
                 total_expenses: { type: :number, description: '費用合計' }
               },
               required: %w[from_date to_date revenues expenses gross_profit operating_income net_income total_revenues total_expenses]

        run_test!
      end

      response(400, 'bad request') do
        let(:from_date) { '2025-12-31' }
        let(:to_date) { '2025-01-01' }

        run_test!
      end
    end
  end

  path '/api/v1/financial-statements/ratios' do
    get('財務指標取得') do
      tags '財務諸表'
      description '財務指標を計算して取得します'
      produces 'application/json'

      parameter name: :as_of_date, in: :query, type: :string, format: :date, description: '基準日（YYYY-MM-DD）', required: true
      parameter name: :from_date, in: :query, type: :string, format: :date, description: '開始日（YYYY-MM-DD）', required: true
      parameter name: :to_date, in: :query, type: :string, format: :date, description: '終了日（YYYY-MM-DD）', required: true

      response(200, 'successful') do
        let(:as_of_date) { '2025-03-31' }
        let(:from_date) { '2025-01-01' }
        let(:to_date) { '2025-03-31' }

        schema type: :object,
               properties: {
                 current_ratio: { type: :number, description: '流動比率（%）' },
                 debt_to_equity_ratio: { type: :number, description: '自己資本比率（%）' },
                 gross_profit_margin: { type: :number, description: '売上総利益率（%）' },
                 operating_profit_margin: { type: :number, description: '営業利益率（%）' },
                 net_profit_margin: { type: :number, description: '当期純利益率（%）' },
                 roa: { type: :number, description: '総資産利益率（ROA, %）' },
                 roe: { type: :number, description: '自己資本利益率（ROE, %）' }
               },
               required: %w[current_ratio debt_to_equity_ratio gross_profit_margin operating_profit_margin net_profit_margin roa roe]

        run_test!
      end

      response(400, 'bad request') do
        let(:as_of_date) { 'invalid-date' }
        let(:from_date) { '2025-01-01' }
        let(:to_date) { '2025-03-31' }

        run_test!
      end
    end
  end
end
