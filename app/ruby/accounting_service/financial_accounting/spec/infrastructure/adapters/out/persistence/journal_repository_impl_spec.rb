# frozen_string_literal: true

require 'rails_helper'
require Rails.root.join('app', 'domain', 'models', 'journal.rb')
require Rails.root.join('app', 'domain', 'models', 'journal_entry.rb')
require Rails.root.join('app', 'ports', 'out', 'journal_repository.rb')
require Rails.root.join('app', 'infrastructure', 'adapters', 'out', 'persistence', 'journal_record.rb')
require Rails.root.join('app', 'infrastructure', 'adapters', 'out', 'persistence', 'journal_entry_record.rb')
require Rails.root.join('app', 'infrastructure', 'adapters', 'out', 'persistence', 'journal_repository_impl.rb')

RSpec.describe Infrastructure::Adapters::Out::Persistence::JournalRepositoryImpl do
  let(:repository) { described_class.new }

  describe '#save' do
    let(:journal) do
      journal = Domain::Models::Journal.new(
        journal_date: Date.parse('2024-01-15'),
        description: '売上計上',
        fiscal_year: 2023
      )
      journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
      journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 100_000))
      journal
    end

    it '仕訳を保存できる' do
      result = repository.save(journal)

      expect(result.journal_id).not_to be_nil
      expect(result.journal_id).to match(/20240115-\d{4}/)

      # データベースに保存されていることを確認
      saved_record = Infrastructure::Adapters::Out::Persistence::JournalRecord.find(result.journal_id)
      expect(saved_record.journal_date).to eq(Date.parse('2024-01-15'))
      expect(saved_record.description).to eq('売上計上')
      expect(saved_record.entry_records.count).to eq(2)
    end
  end

  describe '#find_by_id' do
    let!(:saved_journal) do
      journal = Domain::Models::Journal.new(
        journal_date: Date.parse('2024-01-15'),
        description: '売上計上',
        fiscal_year: 2023
      )
      journal.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 100_000))
      journal.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 100_000))
      repository.save(journal)
    end

    it '保存した仕訳を取得できる' do
      found_journal = repository.find_by_id(saved_journal.journal_id)

      expect(found_journal).not_to be_nil
      expect(found_journal.journal_date).to eq(Date.parse('2024-01-15'))
      expect(found_journal.description).to eq('売上計上')
      expect(found_journal.entries.size).to eq(2)
      expect(found_journal.balanced?).to be true
    end

    it '存在しない仕訳IDの場合はnilを返す' do
      found_journal = repository.find_by_id('99999999-9999')

      expect(found_journal).to be_nil
    end
  end

  describe '#find_by_fiscal_year' do
    before do
      # 2023年度（2023/4/1 - 2024/3/31）のデータ
      journal1 = Domain::Models::Journal.new(
        journal_date: Date.parse('2023-05-15'),
        description: '2023年度仕訳1',
        fiscal_year: 2023
      )
      journal1.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 50_000))
      journal1.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 50_000))
      repository.save(journal1)

      # 2024年度（2024/4/1 - 2025/3/31）のデータ
      journal2 = Domain::Models::Journal.new(
        journal_date: Date.parse('2024-06-15'),
        description: '2024年度仕訳1',
        fiscal_year: 2024
      )
      journal2.add_entry(Domain::Models::JournalEntry.new(account_code: '1001', debit_amount: 75_000))
      journal2.add_entry(Domain::Models::JournalEntry.new(account_code: '4001', credit_amount: 75_000))
      repository.save(journal2)
    end

    it '指定した会計年度の仕訳を取得できる' do
      journals_2023 = repository.find_by_fiscal_year(2023)

      expect(journals_2023.size).to eq(1)
      expect(journals_2023.first.description).to eq('2023年度仕訳1')
    end
  end
end
