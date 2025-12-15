# frozen_string_literal: true

require 'swagger_helper'

RSpec.describe 'api/v1/journal_entries', type: :request do
  let!(:cash_account) { create(:account, code: '1010', name: '現金') }
  let!(:sales_account) { create(:account, :revenue, code: '4010', name: '売上高') }

  path '/api/v1/journal-entries' do
    get('一覧取得') do
      tags '仕訳'
      description 'すべての仕訳を取得します'
      produces 'application/json'

      response(200, 'successful') do
        schema type: :array,
               items: {
                 type: :object,
                 properties: {
                   id: { type: :integer, description: '仕訳ID' },
                   entry_number: { type: :string, description: '仕訳番号' },
                   entry_date: { type: :string, format: :date, description: '仕訳日付' },
                   description: { type: :string, description: '摘要' },
                   total_amount: { type: :number, description: '合計金額' },
                   created_by: { type: :string, description: '作成者' },
                   details: {
                     type: :array,
                     description: '仕訳明細',
                     items: {
                       type: :object,
                       properties: {
                         id: { type: :integer, description: '明細ID' },
                         line_number: { type: :integer, description: '行番号' },
                         account_code: { type: :string, description: '勘定科目コード' },
                         debit_amount: { type: :number, description: '借方金額' },
                         credit_amount: { type: :number, description: '貸方金額' },
                         description: { type: :string, description: '摘要' }
                       },
                       required: %w[id line_number account_code debit_amount credit_amount description]
                     }
                   }
                 },
                 required: %w[id entry_number entry_date description total_amount created_by details]
               }

        run_test!
      end
    end

    post('新規作成') do
      tags '仕訳'
      description '新しい仕訳を作成します。貸借が一致し、明細が2件以上必要です。'
      consumes 'application/json'
      produces 'application/json'

      parameter name: :journal_entry, in: :body, schema: {
        type: :object,
        properties: {
          journal_entry: {
            type: :object,
            properties: {
              entry_number: { type: :string, description: '仕訳番号' },
              entry_date: { type: :string, format: :date, description: '仕訳日付' },
              description: { type: :string, description: '摘要' },
              total_amount: { type: :number, description: '合計金額' },
              created_by: { type: :string, description: '作成者' },
              details_attributes: {
                type: :array,
                description: '仕訳明細',
                items: {
                  type: :object,
                  properties: {
                    line_number: { type: :integer, description: '行番号' },
                    account_code: { type: :string, description: '勘定科目コード' },
                    debit_amount: { type: :number, description: '借方金額' },
                    credit_amount: { type: :number, description: '貸方金額' },
                    description: { type: :string, description: '摘要' }
                  },
                  required: %w[line_number account_code debit_amount credit_amount description]
                }
              }
            },
            required: %w[entry_number entry_date description total_amount created_by details_attributes]
          }
        }
      }

      response(201, 'created') do
        let(:journal_entry) do
          {
            journal_entry: {
              entry_number: 'J999',
              entry_date: Date.new(2025, 1, 15),
              description: 'テスト仕訳',
              total_amount: 10_000,
              created_by: 'test_user',
              details_attributes: [
                {
                  line_number: 1,
                  account_code: cash_account.code,
                  debit_amount: 10_000,
                  credit_amount: 0,
                  description: '現金'
                },
                {
                  line_number: 2,
                  account_code: sales_account.code,
                  debit_amount: 0,
                  credit_amount: 10_000,
                  description: '売上高'
                }
              ]
            }
          }
        end

        run_test!
      end

      response(422, 'unprocessable entity') do
        let(:journal_entry) do
          {
            journal_entry: {
              entry_number: 'J999',
              entry_date: Date.new(2025, 1, 15),
              description: 'テスト仕訳',
              total_amount: 10_000,
              created_by: 'test_user',
              details_attributes: [
                {
                  line_number: 1,
                  account_code: cash_account.code,
                  debit_amount: 10_000,
                  credit_amount: 0,
                  description: '現金'
                },
                {
                  line_number: 2,
                  account_code: sales_account.code,
                  debit_amount: 0,
                  credit_amount: 5_000,
                  description: '売上高'
                }
              ]
            }
          }
        end

        run_test!
      end
    end
  end

  path '/api/v1/journal-entries/{id}' do
    parameter name: 'id', in: :path, type: :integer, description: '仕訳ID'

    get('詳細取得') do
      tags '仕訳'
      description '指定された仕訳を取得します'
      produces 'application/json'

      response(200, 'successful') do
        let(:id) do
          JournalEntry.create!(
            entry_number: 'J001',
            entry_date: Date.new(2025, 1, 15),
            description: 'テスト仕訳',
            total_amount: 10_000,
            created_by: 'test_user'
          ).tap do |entry|
            entry.details.create!(
              line_number: 1,
              account_code: cash_account.code,
              debit_amount: 10_000,
              credit_amount: 0,
              description: '現金'
            )
            entry.details.create!(
              line_number: 2,
              account_code: sales_account.code,
              debit_amount: 0,
              credit_amount: 10_000,
              description: '売上高'
            )
          end.id
        end

        run_test!
      end

      response(404, 'not found') do
        let(:id) { 9999 }

        run_test!
      end
    end

    delete('削除') do
      tags '仕訳'
      description '指定された仕訳を削除します'

      response(204, 'no content') do
        let(:id) do
          JournalEntry.create!(
            entry_number: 'J001',
            entry_date: Date.new(2025, 1, 15),
            description: 'テスト仕訳',
            total_amount: 10_000,
            created_by: 'test_user'
          ).tap do |entry|
            entry.details.create!(
              line_number: 1,
              account_code: cash_account.code,
              debit_amount: 10_000,
              credit_amount: 0,
              description: '現金'
            )
            entry.details.create!(
              line_number: 2,
              account_code: sales_account.code,
              debit_amount: 0,
              credit_amount: 10_000,
              description: '売上高'
            )
          end.id
        end

        run_test!
      end

      response(404, 'not found') do
        let(:id) { 9999 }

        run_test!
      end
    end
  end
end
