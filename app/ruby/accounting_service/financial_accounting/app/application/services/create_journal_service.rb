# frozen_string_literal: true

require_relative '../../ports/in/create_journal_use_case'
require_relative '../../domain/models/journal'
require_relative '../../domain/models/journal_entry'

module Application
  module Services
    class CreateJournalService < Ports::In::CreateJournalUseCase
      def initialize(journal_repository:)
        @journal_repository = journal_repository
      end

      def execute(journal_date:, description:, fiscal_year:, entries:)
        # 1. ドメインモデルの作成
        journal = Domain::Models::Journal.new(
          journal_date: journal_date,
          description: description,
          fiscal_year: fiscal_year
        )

        # 2. 仕訳明細の追加
        entries.each do |entry_data|
          entry = Domain::Models::JournalEntry.new(
            account_code: entry_data[:account_code],
            debit_amount: entry_data[:debit_amount] || 0,
            credit_amount: entry_data[:credit_amount] || 0,
            description: entry_data[:description] || ''
          )
          journal.add_entry(entry)
        end

        # 3. ドメインルールの検証
        journal.validate_balance

        # 4. 永続化
        @journal_repository.save(journal)

        journal
      end
    end
  end
end
