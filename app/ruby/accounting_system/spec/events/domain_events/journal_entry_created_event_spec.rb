# frozen_string_literal: true

require 'rails_helper'

RSpec.describe DomainEvents::JournalEntryCreatedEvent do
  describe '#to_h' do
    it 'イベントデータをハッシュに変換できる' do
      event = described_class.new(
        journal_entry_id: 'journal-123',
        entry_date: Date.parse('2024-01-15'),
        description: '売上計上',
        line_items: [
          { account_code: '1001', debit_or_credit: 'D', amount: 100_000 },
          { account_code: '4001', debit_or_credit: 'C', amount: 100_000 }
        ],
        user_id: 'user-001',
        occurred_at: Time.zone.parse('2024-01-15 10:00:00')
      )

      hash = event.to_h

      expect(hash[:journal_entry_id]).to eq('journal-123')
      expect(hash[:entry_date]).to eq('2024-01-15')
      expect(hash[:description]).to eq('売上計上')
      expect(hash[:line_items].size).to eq(2)
      expect(hash[:user_id]).to eq('user-001')
      expect(hash[:occurred_at]).to eq('2024-01-15T10:00:00Z')
    end
  end

  describe '#to_json' do
    it 'イベントデータをJSONに変換できる' do
      event = described_class.new(
        journal_entry_id: 'journal-123',
        entry_date: Date.parse('2024-01-15'),
        description: '売上計上',
        line_items: [
          { account_code: '1001', debit_or_credit: 'D', amount: 100_000 }
        ],
        user_id: 'user-001',
        occurred_at: Time.zone.parse('2024-01-15 10:00:00')
      )

      json = event.to_json
      parsed = JSON.parse(json)

      expect(parsed['journal_entry_id']).to eq('journal-123')
      expect(parsed['description']).to eq('売上計上')
    end
  end
end
