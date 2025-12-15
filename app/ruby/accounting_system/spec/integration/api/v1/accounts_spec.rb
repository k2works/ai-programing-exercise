# frozen_string_literal: true

require 'swagger_helper'

RSpec.describe 'api/v1/accounts', type: :request do
  path '/api/v1/accounts' do
    get('一覧取得') do
      tags '勘定科目'
      description 'すべての勘定科目を取得します'
      produces 'application/json'

      response(200, 'successful') do
        schema type: :array,
               items: {
                 type: :object,
                 properties: {
                   code: { type: :string, description: '勘定科目コード' },
                   name: { type: :string, description: '勘定科目名' },
                   account_type: { type: :string, description: '勘定科目種別', enum: %w[asset liability equity revenue expense] },
                   bspl_type: { type: :string, description: 'BSPL区分', enum: %w[B P] },
                   debit_credit_type: { type: :string, description: '貸借区分', enum: %w[D C] },
                   transaction_type: { type: :string, description: '取引要素区分' },
                   expense_type: { type: :string, description: '費用区分', nullable: true },
                   tax_code: { type: :string, description: '課税取引コード', nullable: true },
                   display_order: { type: :integer, description: '表示順序' },
                   is_summary: { type: :boolean, description: '集計科目フラグ' }
                 },
                 required: %w[code name account_type bspl_type debit_credit_type transaction_type display_order is_summary]
               }

        run_test!
      end
    end

    post('新規作成') do
      tags '勘定科目'
      description '新しい勘定科目を作成します'
      consumes 'application/json'
      produces 'application/json'

      parameter name: :account, in: :body, schema: {
        type: :object,
        properties: {
          account: {
            type: :object,
            properties: {
              code: { type: :string, description: '勘定科目コード' },
              name: { type: :string, description: '勘定科目名' },
              account_type: { type: :string, description: '勘定科目種別', enum: %w[asset liability equity revenue expense] },
              bspl_type: { type: :string, description: 'BSPL区分', enum: %w[B P] },
              debit_credit_type: { type: :string, description: '貸借区分', enum: %w[D C] },
              transaction_type: { type: :string, description: '取引要素区分' },
              expense_type: { type: :string, description: '費用区分', nullable: true },
              tax_code: { type: :string, description: '課税取引コード', nullable: true },
              display_order: { type: :integer, description: '表示順序' },
              is_summary: { type: :boolean, description: '集計科目フラグ' }
            },
            required: %w[code name account_type bspl_type debit_credit_type transaction_type display_order is_summary]
          }
        }
      }

      response(201, 'created') do
        let(:account) do
          {
            account: {
              code: '9999',
              name: 'テスト科目',
              account_type: 'asset',
              bspl_type: 'B',
              debit_credit_type: 'D',
              transaction_type: '1',
              display_order: 100,
              is_summary: false
            }
          }
        end

        run_test!
      end

      response(422, 'unprocessable entity') do
        let(:account) do
          {
            account: {
              code: '',
              name: 'テスト科目'
            }
          }
        end

        run_test!
      end
    end
  end

  path '/api/v1/accounts/{code}' do
    parameter name: 'code', in: :path, type: :string, description: '勘定科目コード'

    get('詳細取得') do
      tags '勘定科目'
      description '指定された勘定科目を取得します'
      produces 'application/json'

      response(200, 'successful') do
        let(:code) { create(:account).code }

        run_test!
      end

      response(404, 'not found') do
        let(:code) { '9999' }

        run_test!
      end
    end

    put('更新') do
      tags '勘定科目'
      description '指定された勘定科目を更新します'
      consumes 'application/json'
      produces 'application/json'

      parameter name: :account, in: :body, schema: {
        type: :object,
        properties: {
          account: {
            type: :object,
            properties: {
              name: { type: :string, description: '勘定科目名' },
              display_order: { type: :integer, description: '表示順序' }
            }
          }
        }
      }

      response(200, 'successful') do
        let(:code) { create(:account).code }
        let(:account) do
          {
            account: {
              name: '更新後の名前'
            }
          }
        end

        run_test!
      end

      response(404, 'not found') do
        let(:code) { '9999' }
        let(:account) do
          {
            account: {
              name: '更新後の名前'
            }
          }
        end

        run_test!
      end
    end

    delete('削除') do
      tags '勘定科目'
      description '指定された勘定科目を削除します'

      response(204, 'no content') do
        let(:code) { create(:account).code }

        run_test!
      end

      response(404, 'not found') do
        let(:code) { '9999' }

        run_test!
      end
    end
  end
end
