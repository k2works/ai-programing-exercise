# frozen_string_literal: true

module Infrastructure
  module Adapters
    module Out
      module External
        class FinancialAccountingClient
          BASE_URL = ENV.fetch('FINANCIAL_ACCOUNTING_SERVICE_URL', 'http://localhost:3000')

          def fetch_financial_data_by_fiscal_year(fiscal_year)
            # 1. 財務会計サービスから仕訳データを取得
            journals = fetch_journals(fiscal_year)

            # 2. 管理会計ドメインモデルに変換（腐敗防止層）
            # 簡易実装: 仕訳データから財務データを計算
            convert_to_financial_data(fiscal_year, journals)
          end

          private

          def fetch_journals(fiscal_year)
            conn = Faraday.new(url: BASE_URL) do |f|
              f.request :json
              f.response :json, content_type: /\bjson$/
              f.adapter Faraday.default_adapter
            end

            response = conn.get('/api/journals', { fiscal_year: fiscal_year })
            response.body
          end

          def convert_to_financial_data(fiscal_year, journals)
            # 仕訳データから財務指標を計算
            sales = calculate_sales(journals)
            cost_of_sales = calculate_cost_of_sales(journals)
            selling_general_admin = calculate_selling_general_admin(journals)
            operating_profit = sales - cost_of_sales - selling_general_admin

            total_assets = calculate_total_assets(journals)
            tangible_fixed_assets = calculate_tangible_fixed_assets(journals)
            current_assets = calculate_current_assets(journals)
            current_liabilities = calculate_current_liabilities(journals)
            quick_assets = calculate_quick_assets(journals)
            equity = calculate_equity(journals)

            # 財務会計のデータ構造を管理会計のドメインモデルに変換
            Domain::Models::FinancialData.new(
              fiscal_year: fiscal_year,
              sales: sales,
              operating_profit: operating_profit,
              total_assets: total_assets,
              tangible_fixed_assets: tangible_fixed_assets,
              current_assets: current_assets,
              current_liabilities: current_liabilities,
              quick_assets: quick_assets,
              equity: equity
            )
          end

          def calculate_sales(journals)
            # 売上高（勘定科目コード: 4110）
            journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '4110' ? entry['credit_amount'] : 0
              end
            end
          end

          def calculate_cost_of_sales(journals)
            # 売上原価（勘定科目コード: 5110）
            journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '5110' ? entry['debit_amount'] : 0
              end
            end
          end

          def calculate_selling_general_admin(journals)
            # 販売費及び一般管理費（勘定科目コード: 5210）
            journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '5210' ? entry['debit_amount'] : 0
              end
            end
          end

          def calculate_total_assets(journals)
            # 総資産（資産勘定の合計）
            calculate_current_assets(journals) + calculate_tangible_fixed_assets(journals)
          end

          def calculate_tangible_fixed_assets(journals)
            # 有形固定資産（勘定科目コード: 1210）
            journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '1210' ? entry['debit_amount'] : 0
              end
            end
          end

          def calculate_current_assets(journals)
            # 流動資産（現金預金 + 売掛金 + 棚卸資産）
            cash_and_deposits = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '1110' ? entry['debit_amount'] - entry['credit_amount'] : 0
              end
            end

            accounts_receivable = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '1120' ? entry['debit_amount'] : 0
              end
            end

            inventory = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '1140' ? entry['debit_amount'] : 0
              end
            end

            cash_and_deposits + accounts_receivable + inventory
          end

          def calculate_current_liabilities(journals)
            # 流動負債（買掛金 + 短期借入金）
            accounts_payable = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '2110' ? entry['credit_amount'] : 0
              end
            end

            short_term_debt = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '2120' ? entry['credit_amount'] : 0
              end
            end

            accounts_payable + short_term_debt
          end

          def calculate_quick_assets(journals)
            # 当座資産（現金預金 + 売掛金）
            cash_and_deposits = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '1110' ? entry['debit_amount'] - entry['credit_amount'] : 0
              end
            end

            accounts_receivable = journals.sum do |journal|
              journal['entries'].sum do |entry|
                entry['account_code'] == '1120' ? entry['debit_amount'] : 0
              end
            end

            cash_and_deposits + accounts_receivable
          end

          def calculate_equity(journals)
            # 純資産（総資産 - 総負債）
            calculate_total_assets(journals) - calculate_current_liabilities(journals)
          end
        end
      end
    end
  end
end
