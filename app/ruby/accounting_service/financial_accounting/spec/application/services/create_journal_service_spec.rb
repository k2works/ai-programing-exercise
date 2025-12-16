# frozen_string_literal: true

require 'date'
require 'bigdecimal'
require_relative '../../../app/application/services/create_journal_service'
require_relative '../../../app/ports/out/journal_repository'

RSpec.describe Application::Services::CreateJournalService do
  let(:journal_repository) { instance_double(Ports::Out::JournalRepository) }
  let(:service) { described_class.new(journal_repository: journal_repository) }

  describe '#execute' do
    let(:journal_date) { Date.parse('2024-01-15') }
    let(:description) { '売上計上' }
    let(:fiscal_year) { 2024 }
    let(:entries) do
      [
        { account_code: '1001', debit_amount: 100_000, description: '現金' },
        { account_code: '4001', credit_amount: 100_000, description: '売上' }
      ]
    end

    context '有効な仕訳データの場合' do
      it '仕訳を作成して保存できる' do
        expect(journal_repository).to receive(:save) do |journal|
          expect(journal).to be_a(Domain::Models::Journal)
          expect(journal.journal_date).to eq(journal_date)
          expect(journal.description).to eq(description)
          expect(journal.entries.size).to eq(2)
          journal
        end

        result = service.execute(
          journal_date: journal_date,
          description: description,
          fiscal_year: fiscal_year,
          entries: entries
        )

        expect(result.balanced?).to be true
      end
    end

    context '貸借が一致しない仕訳データの場合' do
      let(:unbalanced_entries) do
        [
          { account_code: '1001', debit_amount: 100_000 },
          { account_code: '4001', credit_amount: 50_000 }
        ]
      end

      it 'エラーが発生する' do
        expect {
          service.execute(
            journal_date: journal_date,
            description: description,
            fiscal_year: fiscal_year,
            entries: unbalanced_entries
          )
        }.to raise_error(StandardError, /貸借が一致しません/)
      end
    end
  end
end
