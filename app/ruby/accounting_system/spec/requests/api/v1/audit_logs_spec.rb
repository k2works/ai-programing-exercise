# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Api::V1::AuditLogs', type: :request do
  let(:base_time) { Time.current }
  let!(:account_log) do
    create(:audit_log, entity_type: 'Account', entity_id: '1001', user_id: 'user001', timestamp: base_time)
  end
  let!(:journal_log) do
    create(:audit_log, entity_type: 'Journal', entity_id: '12345', user_id: 'user002', timestamp: base_time)
  end

  describe 'GET /api/v1/audit_logs/entity/:entity_type/:entity_id' do
    it 'エンティティの変更履歴を取得できる' do
      get '/api/v1/audit_logs/entity/Account/1001'

      expect(response).to have_http_status(:ok)
      json = JSON.parse(response.body)
      expect(json.size).to eq(1)
      expect(json[0]['entity_type']).to eq('Account')
      expect(json[0]['entity_id']).to eq('1001')
    end
  end

  describe 'GET /api/v1/audit_logs/user/:user_id' do
    it 'ユーザーの操作履歴を取得できる' do
      start_date = base_time - 1.hour
      end_date = base_time + 1.hour

      get '/api/v1/audit_logs/user/user001', params: {
        start_date: start_date.iso8601,
        end_date: end_date.iso8601
      }

      expect(response).to have_http_status(:ok)
      json = JSON.parse(response.body)
      expect(json.size).to eq(1)
      expect(json[0]['user_id']).to eq('user001')
    end
  end

  describe 'GET /api/v1/audit_logs/period' do
    it '期間別の監査ログを取得できる' do
      start_date = base_time - 1.hour
      end_date = base_time + 1.hour

      get '/api/v1/audit_logs/period', params: {
        start_date: start_date.iso8601,
        end_date: end_date.iso8601,
        limit: 100
      }

      expect(response).to have_http_status(:ok)
      json = JSON.parse(response.body)
      expect(json.size).to eq(2)
    end
  end
end
