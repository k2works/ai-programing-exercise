# frozen_string_literal: true

require 'bigdecimal'
require 'date'
require_relative '../../../app/domain/models/journal'
require_relative '../../../app/domain/models/journal_entry'

RSpec.describe Domain::Models::Journal do
  let(:journal_date) { Date.parse('2024-01-15') }
  let(:description) { '売上計上' }
  let(:fiscal_year) { 2024 }
  let(:journal) { described_class.new(journal_date: journal_date, description: description, fiscal_year: fiscal_year) }

  describe '#initialize' do
    it '仕訳を初期化できる' do
      expect(journal.journal_date).to eq(journal_date)
      expect(journal.description).to eq(description)
      expect(journal.fiscal_year).to eq(fiscal_year)
      expect(journal.entries).to eq([])
    end
  end

  describe '#add_entry' do
    context '有効な明細の場合' do
      it '明細を追加できる' do
        entry = Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000, description: '現金')
        journal.add_entry(entry)
        expect(journal.entries.size).to eq(1)
      end
    end

    context '無効な明細の場合' do
      it '借方・貸方がともに0の場合はエラーになる' do
        entry = Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 0, credit_amount: 0)
        expect { journal.add_entry(entry) }.to raise_error(ArgumentError, '借方・貸方の少なくとも一方は0より大きい必要があります')
      end
    end
  end

  describe '#balanced?' do
    context '貸借が一致する場合' do
      it 'true を返す' do
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 100_000))
        expect(journal.balanced?).to be true
      end
    end

    context '貸借が一致しない場合' do
      it 'false を返す' do
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 50_000))
        expect(journal.balanced?).to be false
      end
    end
  end

  describe '#validate_balance' do
    context '貸借が一致する場合' do
      it 'エラーが発生しない' do
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 100_000))
        expect { journal.validate_balance }.not_to raise_error
      end
    end

    context '貸借が一致しない場合' do
      it 'エラーが発生する' do
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
        journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 50_000))
        expect { journal.validate_balance }.to raise_error(StandardError, /貸借が一致しません/)
      end
    end
  end
end
