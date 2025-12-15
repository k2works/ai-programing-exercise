# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Api::V1::JournalEntries', type: :request do
  let!(:cash_account) { create(:cash_account) }
  let!(:sales_account) { create(:account, :revenue, code: '4010', name: '売上高') }

  describe 'GET /api/v1/journal-entries' do
    it '仕訳一覧を取得できる' do
      # Given: 仕訳を作成
      create_journal_entry('J001')
      create_journal_entry('J002')

      # When: 一覧を取得
      get '/api/v1/journal-entries'

      # Then: 正常なレスポンスが返る
      expect(response).to have_http_status(:ok)
      expect(response.content_type).to include('application/json')

      json = JSON.parse(response.body)
      expect(json.size).to be >= 2
      expect(json[0]).to have_key('id')
      expect(json[0]).to have_key('entry_number')
      expect(json[0]).to have_key('details')
    end

    it '仕訳が存在しない場合は空配列を返す' do
      # When: 仕訳が存在しない状態で取得
      get '/api/v1/journal-entries'

      # Then: 空配列が返る
      expect(response).to have_http_status(:ok)
      json = JSON.parse(response.body)
      expect(json).to eq([])
    end
  end

  describe 'GET /api/v1/journal-entries/:id' do
    context '仕訳が存在する場合' do
      let!(:entry) { create_journal_entry('J001') }

      it '仕訳を取得できる' do
        # When: IDで取得
        get "/api/v1/journal-entries/#{entry.id}"

        # Then: 正常なレスポンスが返る
        expect(response).to have_http_status(:ok)

        json = JSON.parse(response.body)
        expect(json['id']).to eq(entry.id)
        expect(json['entry_number']).to eq('J001')
        expect(json['details']).to be_a(Array)
        expect(json['details'].size).to eq(2)
      end
    end

    context '仕訳が存在しない場合' do
      it '404エラーが返る' do
        # When: 存在しないIDで取得
        get '/api/v1/journal-entries/9999'

        # Then: 404エラー
        expect(response).to have_http_status(:not_found)

        json = JSON.parse(response.body)
        expect(json['error']).to include('9999')
      end
    end
  end

  describe 'POST /api/v1/journal-entries' do
    context '正しいパラメータの場合' do
      let(:valid_attributes) do
        {
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
      end

      it '仕訳を作成できる' do
        # When: 新規作成
        expect do
          post '/api/v1/journal-entries', params: { journal_entry: valid_attributes }
        end.to change(JournalEntry, :count).by(1)

        # Then: 201 Createdが返る
        expect(response).to have_http_status(:created)

        json = JSON.parse(response.body)
        expect(json['entry_number']).to eq('J999')
        expect(json['details'].size).to eq(2)
      end
    end

    context '貸借が一致しない場合' do
      let(:invalid_attributes) do
        {
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
      end

      it 'バリデーションエラーが発生する' do
        # When: 貸借が一致しない仕訳を作成
        post '/api/v1/journal-entries', params: { journal_entry: invalid_attributes }

        # Then: 422 Unprocessable Entityが返る
        expect(response).to have_http_status(:unprocessable_entity)

        json = JSON.parse(response.body)
        expect(json['error']).to include('貸借が一致していません')
      end
    end

    context '明細が2件未満の場合' do
      let(:invalid_attributes) do
        {
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
            }
          ]
        }
      end

      it 'バリデーションエラーが発生する' do
        # When: 明細が1件の仕訳を作成
        post '/api/v1/journal-entries', params: { journal_entry: invalid_attributes }

        # Then: 422 Unprocessable Entityが返る
        expect(response).to have_http_status(:unprocessable_entity)

        json = JSON.parse(response.body)
        expect(json['error']).to include('仕訳明細は2件以上必要です')
      end
    end
  end

  describe 'DELETE /api/v1/journal-entries/:id' do
    context '仕訳が存在する場合' do
      let!(:entry) { create_journal_entry('J001') }

      it '仕訳を削除できる' do
        # When: 削除
        expect do
          delete "/api/v1/journal-entries/#{entry.id}"
        end.to change(JournalEntry, :count).by(-1)

        # Then: 204 No Contentが返る
        expect(response).to have_http_status(:no_content)

        # レスポンスボディは空
        expect(response.body).to be_empty
      end
    end

    context '仕訳が存在しない場合' do
      it '404エラーが返る' do
        # When: 存在しないIDで削除
        delete '/api/v1/journal-entries/9999'

        # Then: 404エラーが返る
        expect(response).to have_http_status(:not_found)
      end
    end
  end

  private

  def create_journal_entry(entry_number)
    JournalEntry.create!(
      entry_number: entry_number,
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
    end
  end
end
