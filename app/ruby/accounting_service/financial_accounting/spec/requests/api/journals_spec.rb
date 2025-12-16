# frozen_string_literal: true

require 'rails_helper'

# ドメインモデルとインフラストラクチャを明示的にロード
require Rails.root.join('app', 'domain', 'models', 'journal.rb')
require Rails.root.join('app', 'domain', 'models', 'journal_entry.rb')
require Rails.root.join('app', 'ports', 'in', 'create_journal_use_case.rb')
require Rails.root.join('app', 'ports', 'out', 'journal_repository.rb')
require Rails.root.join('app', 'application', 'services', 'create_journal_service.rb')
require Rails.root.join('app', 'infrastructure', 'adapters', 'out', 'persistence', 'journal_record.rb')
require Rails.root.join('app', 'infrastructure', 'adapters', 'out', 'persistence', 'journal_entry_record.rb')
require Rails.root.join('app', 'infrastructure', 'adapters', 'out', 'persistence', 'journal_repository_impl.rb')

RSpec.describe 'Api::Journals', type: :request do
  let(:repository) { Infrastructure::Adapters::Out::Persistence::JournalRepositoryImpl.new }

  before do
    # テストデータの作成
    journal1 = Domain::Models::Journal.new(
      journal_date: Date.parse('2023-05-15'),
      description: '2023年度仕訳1',
      fiscal_year: 2023
    )
    journal1.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 50_000))
    journal1.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 50_000))
    repository.save(journal1)

    journal2 = Domain::Models::Journal.new(
      journal_date: Date.parse('2024-06-15'),
      description: '2024年度仕訳1',
      fiscal_year: 2024
    )
    journal2.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 75_000))
    journal2.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 75_000))
    repository.save(journal2)
  end

  describe 'GET /api/journals' do
    context '会計年度を指定した場合' do
      it '指定した会計年度の仕訳一覧を返す' do
        get '/api/journals', params: { fiscal_year: 2023 }

        expect(response).to have_http_status(:ok)
        json = JSON.parse(response.body)
        expect(json.size).to eq(1)
        expect(json.first['description']).to eq('2023年度仕訳1')
      end
    end

    context '会計年度を指定しない場合' do
      it 'すべての仕訳一覧を返す' do
        get '/api/journals'

        expect(response).to have_http_status(:ok)
        json = JSON.parse(response.body)
        expect(json.size).to eq(2)
      end
    end
  end

  describe 'GET /api/journals/:id' do
    let(:saved_journal) do
      journal = Domain::Models::Journal.new(
        journal_date: Date.parse('2023-07-01'),
        description: 'テスト仕訳',
        fiscal_year: 2023
      )
      journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
      journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 100_000))
      repository.save(journal)
    end

    it '指定したIDの仕訳を返す' do
      get "/api/journals/#{saved_journal.journal_id}"

      expect(response).to have_http_status(:ok)
      json = JSON.parse(response.body)
      expect(json['journal_id']).to eq(saved_journal.journal_id)
      expect(json['description']).to eq('テスト仕訳')
      expect(json['entries'].size).to eq(2)
    end

    it '存在しないIDの場合は404を返す' do
      get '/api/journals/99999999-9999'

      expect(response).to have_http_status(:not_found)
    end
  end

  describe 'POST /api/journals' do
    let(:journal_params) do
      {
        journal_date: '2023-08-15',
        description: '新規仕訳',
        fiscal_year: 2023,
        entries: [
          { account_code: '1001', debit_amount: 200_000, credit_amount: 0, description: '現金' },
          { account_code: '4001', debit_amount: 0, credit_amount: 200_000, description: '売上' }
        ]
      }
    end

    it '仕訳を作成して201を返す' do
      post '/api/journals', params: journal_params, as: :json

      if response.status != 201
        puts "Error response: #{response.body}"
      end

      expect(response).to have_http_status(:created)
      json = JSON.parse(response.body)
      expect(json['description']).to eq('新規仕訳')
      expect(json['entries'].size).to eq(2)
    end

    it '貸借が一致しない場合は422を返す' do
      invalid_params = journal_params.dup
      invalid_params[:entries][1][:credit_amount] = 100_000

      post '/api/journals', params: invalid_params, as: :json

      expect(response).to have_http_status(:unprocessable_content)
    end
  end
end
