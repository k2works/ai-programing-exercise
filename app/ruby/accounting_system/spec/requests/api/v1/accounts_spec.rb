# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Api::V1::Accounts', type: :request do
  describe 'GET /api/v1/accounts' do
    it '勘定科目一覧を取得できる' do
      # Given: 複数の勘定科目を作成
      create(:cash_account)
      create(:accounts_payable)

      # When: 一覧を取得
      get '/api/v1/accounts'

      # Then: 正常なレスポンスが返る
      expect(response).to have_http_status(:ok)
      expect(response.content_type).to include('application/json')

      json = JSON.parse(response.body)
      expect(json.size).to be >= 2
      expect(json[0]).to have_key('code')
      expect(json[0]).to have_key('name')
    end

    it '勘定科目が存在しない場合は空配列を返す' do
      # When: 勘定科目が存在しない状態で取得
      get '/api/v1/accounts'

      # Then: 空配列が返る
      expect(response).to have_http_status(:ok)
      json = JSON.parse(response.body)
      expect(json).to eq([])
    end
  end

  describe 'GET /api/v1/accounts/:code' do
    context '勘定科目が存在する場合' do
      let!(:account) { create(:cash_account) }

      it '勘定科目を取得できる' do
        # When: 科目コードで取得
        get "/api/v1/accounts/#{account.code}"

        # Then: 正常なレスポンスが返る
        expect(response).to have_http_status(:ok)

        json = JSON.parse(response.body)
        expect(json['code']).to eq(account.code)
        expect(json['name']).to eq(account.name)
        expect(json['account_type']).to eq(account.account_type)
      end
    end

    context '勘定科目が存在しない場合' do
      it '404エラーが返る' do
        # When: 存在しない科目コードで取得
        get '/api/v1/accounts/9999'

        # Then: 404エラー
        expect(response).to have_http_status(:not_found)

        json = JSON.parse(response.body)
        expect(json['error']).to include('9999')
      end
    end
  end

  describe 'POST /api/v1/accounts' do
    context '正しいパラメータの場合' do
      let(:valid_attributes) do
        {
          code: '9999',
          name: 'テスト勘定科目',
          account_type: 'asset',
          bspl_type: 'B',
          debit_credit_type: 'D',
          transaction_type: '1',
          display_order: 100
        }
      end

      it '勘定科目を作成できる' do
        # When: 新規作成
        expect do
          post '/api/v1/accounts', params: { account: valid_attributes }
        end.to change(Account, :count).by(1)

        # Then: 201 Createdが返る
        expect(response).to have_http_status(:created)

        json = JSON.parse(response.body)
        expect(json['code']).to eq('9999')
        expect(json['name']).to eq('テスト勘定科目')
      end
    end

    context '不正なパラメータの場合' do
      let(:invalid_attributes) do
        {
          code: '',
          name: '',
          bspl_type: 'X'
        }
      end

      it 'バリデーションエラーが発生する' do
        # When: 不正なパラメータで作成
        post '/api/v1/accounts', params: { account: invalid_attributes }

        # Then: 422 Unprocessable Entityが返る
        expect(response).to have_http_status(:unprocessable_entity)

        json = JSON.parse(response.body)
        expect(json['error']).to be_present
      end
    end

    context '重複する科目コードの場合' do
      let!(:existing_account) { create(:account, code: '9999') }
      let(:duplicate_attributes) do
        {
          code: '9999',
          name: '重複科目',
          account_type: 'asset',
          bspl_type: 'B',
          debit_credit_type: 'D',
          transaction_type: '1'
        }
      end

      it '409 Conflictエラーが返る' do
        # When: 重複する科目コードで作成
        post '/api/v1/accounts', params: { account: duplicate_attributes }

        # Then: 409 Conflictが返る
        expect(response).to have_http_status(:conflict)

        json = JSON.parse(response.body)
        expect(json['error']).to include('9999')
      end
    end
  end

  describe 'PUT /api/v1/accounts/:code' do
    let!(:account) { create(:cash_account) }

    context '正しいパラメータの場合' do
      it '勘定科目を更新できる' do
        # When: 更新
        put "/api/v1/accounts/#{account.code}",
            params: { account: { name: '更新された現金' } }

        # Then: 正常なレスポンスが返る
        expect(response).to have_http_status(:ok)

        json = JSON.parse(response.body)
        expect(json['name']).to eq('更新された現金')

        # DBが更新されている
        account.reload
        expect(account.name).to eq('更新された現金')
      end
    end

    context '不正なパラメータの場合' do
      it 'バリデーションエラーが発生する' do
        # When: 不正なBSPL区分で更新
        put "/api/v1/accounts/#{account.code}",
            params: { account: { bspl_type: 'X' } }

        # Then: 422 Unprocessable Entityが返る
        expect(response).to have_http_status(:unprocessable_entity)
      end
    end

    context '勘定科目が存在しない場合' do
      it '404エラーが返る' do
        # When: 存在しない科目コードで更新
        put '/api/v1/accounts/9999',
            params: { account: { name: 'テスト' } }

        # Then: 404エラーが返る
        expect(response).to have_http_status(:not_found)
      end
    end
  end

  describe 'DELETE /api/v1/accounts/:code' do
    context '勘定科目が存在する場合' do
      let!(:account) { create(:cash_account) }

      it '勘定科目を削除できる' do
        # When: 削除
        expect do
          delete "/api/v1/accounts/#{account.code}"
        end.to change(Account, :count).by(-1)

        # Then: 204 No Contentが返る
        expect(response).to have_http_status(:no_content)

        # レスポンスボディは空
        expect(response.body).to be_empty
      end
    end

    context '勘定科目が存在しない場合' do
      it '404エラーが返る' do
        # When: 存在しない科目コードで削除
        delete '/api/v1/accounts/9999'

        # Then: 404エラーが返る
        expect(response).to have_http_status(:not_found)
      end
    end
  end
end
