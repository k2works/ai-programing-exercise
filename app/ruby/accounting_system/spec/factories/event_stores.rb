# frozen_string_literal: true

FactoryBot.define do
  factory :event_store do
    aggregate_id { 'account-1001' }
    aggregate_type { 'Account' }
    event_type { 'AccountCreatedEvent' }
    event_version { 1 }
    event_data do
      {
        account_id: '1001',
        name: '現金',
        account_type: 'ASSET',
        initial_balance: 0
      }
    end
    sequence_number { 1 }
    occurred_at { Time.current }
    user_id { 'user-123' }
    correlation_id { SecureRandom.uuid }

    trait :balance_increased do
      event_type { 'BalanceIncreasedEvent' }
      event_data do
        {
          account_id: '1001',
          amount: 100_000,
          reason: '売上'
        }
      end
      sequence_number { 2 }
    end

    trait :balance_decreased do
      event_type { 'BalanceDecreasedEvent' }
      event_data do
        {
          account_id: '1001',
          amount: 50_000,
          reason: '支払い'
        }
      end
      sequence_number { 3 }
    end

    trait :journal_entry do
      aggregate_id { 'journal-12345' }
      aggregate_type { 'JournalEntry' }
      event_type { 'JournalCreatedEvent' }
      event_data do
        {
          journal_id: '12345',
          journal_date: '2024-01-15',
          description: '売上計上',
          details: [
            { account_code: '1001', debit_or_credit: 'D', amount: 100_000 },
            { account_code: '4001', debit_or_credit: 'C', amount: 100_000 }
          ]
        }
      end
    end
  end
end
