# frozen_string_literal: true

# インフラストラクチャ層：Input Adapter
# 勘定科目 REST API コントローラー
module Api
  module V1
    class AccountsController < ApplicationController
      before_action :set_account_service

      # GET /api/v1/accounts
      # すべての勘定科目を取得
      def index
        accounts = @account_service.all_accounts
        render json: accounts.map { |account| AccountSerializer.new(account).as_json }
      end

      # GET /api/v1/accounts/:code
      # 科目コードで勘定科目を取得
      def show
        account = @account_service.find_account(params[:code])
        render json: AccountSerializer.new(account).as_json
      rescue AccountNotFoundError => e
        render json: { error: e.message }, status: :not_found
      end

      # POST /api/v1/accounts
      # 新しい勘定科目を作成
      def create
        account = @account_service.create_account(account_params)
        render json: AccountSerializer.new(account).as_json, status: :created
      rescue DuplicateAccountError => e
        render json: { error: e.message }, status: :conflict
      rescue ArgumentError => e
        render json: { error: e.message }, status: :unprocessable_entity
      end

      # PUT /api/v1/accounts/:code
      # 勘定科目を更新
      def update
        account = @account_service.update_account(params[:code], account_params)
        render json: AccountSerializer.new(account).as_json
      rescue AccountNotFoundError => e
        render json: { error: e.message }, status: :not_found
      rescue ArgumentError => e
        render json: { error: e.message }, status: :unprocessable_entity
      end

      # DELETE /api/v1/accounts/:code
      # 勘定科目を削除
      def destroy
        @account_service.delete_account(params[:code])
        head :no_content
      rescue AccountNotFoundError => e
        render json: { error: e.message }, status: :not_found
      end

      private

      def set_account_service
        @account_service = AccountService.new
      end

      def account_params
        params.require(:account).permit(
          :code,
          :name,
          :account_type,
          :bspl_type,
          :debit_credit_type,
          :transaction_type,
          :expense_type,
          :tax_code,
          :display_order,
          :is_summary
        )
      end
    end
  end
end
