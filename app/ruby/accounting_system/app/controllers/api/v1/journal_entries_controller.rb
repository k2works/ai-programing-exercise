# frozen_string_literal: true

# インフラストラクチャ層：Input Adapter
# 仕訳 REST API コントローラー
module Api
  module V1
    class JournalEntriesController < ApplicationController
      before_action :set_journal_entry_service

      # GET /api/v1/journal-entries
      # すべての仕訳を取得
      def index
        entries = @journal_entry_service.all_journal_entries
        render json: entries.map { |entry| JournalEntrySerializer.new(entry).as_json }
      end

      # GET /api/v1/journal-entries/:id
      # IDで仕訳を取得
      def show
        entry = @journal_entry_service.find_journal_entry(params[:id])
        render json: JournalEntrySerializer.new(entry).as_json
      rescue JournalEntryNotFoundError => e
        render json: { error: e.message }, status: :not_found
      end

      # POST /api/v1/journal-entries
      # 新しい仕訳を作成
      def create
        entry = @journal_entry_service.create_journal_entry(journal_entry_params)
        render json: JournalEntrySerializer.new(entry).as_json, status: :created
      rescue InvalidJournalEntryError, AccountNotFoundError => e
        render json: { error: e.message }, status: :unprocessable_entity
      end

      # DELETE /api/v1/journal-entries/:id
      # 仕訳を削除
      def destroy
        @journal_entry_service.delete_journal_entry(params[:id])
        head :no_content
      rescue JournalEntryNotFoundError => e
        render json: { error: e.message }, status: :not_found
      end

      private

      def set_journal_entry_service
        @journal_entry_service = JournalEntryService.new
      end

      def journal_entry_params
        params.require(:journal_entry).permit(
          :entry_number,
          :entry_date,
          :description,
          :total_amount,
          :created_by,
          details_attributes: [
            :line_number,
            :account_code,
            :debit_amount,
            :credit_amount,
            :description
          ]
        )
      end
    end
  end
end
