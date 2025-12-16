# frozen_string_literal: true

require_relative '../../infrastructure/adapters/out/persistence/journal_repository_impl'
require_relative '../../application/services/create_journal_service'

module Api
  class JournalsController < ApplicationController
    def index
      journals = if params[:fiscal_year]
                   repository.find_by_fiscal_year(params[:fiscal_year].to_i)
                 else
                   repository.find_all
                 end

      render json: journals.map { |journal| journal_to_hash(journal) }
    end

    def show
      journal = repository.find_by_id(params[:id])

      if journal
        render json: journal_to_hash(journal)
      else
        render json: { error: 'Journal not found' }, status: :not_found
      end
    end

    def create
      permitted_params = journal_params

      journal = create_journal_service.execute(
        journal_date: Date.parse(permitted_params[:journal_date]),
        description: permitted_params[:description],
        fiscal_year: permitted_params[:fiscal_year],
        entries: permitted_params[:entries].map(&:to_h)
      )

      render json: journal_to_hash(journal), status: :created
    rescue StandardError => e
      render json: { error: e.message }, status: :unprocessable_content
    end

    private

    def repository
      @repository ||= Infrastructure::Adapters::Out::Persistence::JournalRepositoryImpl.new
    end

    def create_journal_service
      @create_journal_service ||= Application::Services::CreateJournalService.new(journal_repository: repository)
    end

    def journal_params
      params.permit(
        :journal_date,
        :description,
        :fiscal_year,
        entries: [:account_code, :debit_amount, :credit_amount, :description]
      )
    end

    def journal_to_hash(journal)
      {
        journal_id: journal.journal_id,
        journal_date: journal.journal_date.to_s,
        description: journal.description,
        fiscal_year: journal.fiscal_year,
        entries: journal.entries.map do |entry|
          {
            account_code: entry.account_code,
            debit_amount: entry.debit_amount.to_f,
            credit_amount: entry.credit_amount.to_f,
            description: entry.description
          }
        end
      }
    end
  end
end
