# frozen_string_literal: true

require 'rails_helper'

RSpec.describe EventStore, type: :model do
  describe 'バリデーション' do
    it '有効なイベントストアを作成できる' do
      event_store = build(:event_store)
      expect(event_store).to be_valid
    end

    it 'aggregate_id が必須である' do
      event_store = build(:event_store, aggregate_id: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:aggregate_id]).to include("can't be blank")
    end

    it 'aggregate_type が必須である' do
      event_store = build(:event_store, aggregate_type: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:aggregate_type]).to include("can't be blank")
    end

    it 'event_type が必須である' do
      event_store = build(:event_store, event_type: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:event_type]).to include("can't be blank")
    end

    it 'event_version が必須である' do
      event_store = build(:event_store, event_version: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:event_version]).to include("can't be blank")
    end

    it 'event_data が必須である' do
      event_store = build(:event_store, event_data: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:event_data]).to include("can't be blank")
    end

    it 'sequence_number が必須である' do
      event_store = build(:event_store, sequence_number: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:sequence_number]).to include("can't be blank")
    end

    it 'occurred_at が必須である' do
      event_store = build(:event_store, occurred_at: nil)
      expect(event_store).not_to be_valid
      expect(event_store.errors[:occurred_at]).to include("can't be blank")
    end
  end

  describe '不変性' do
    it '保存後は readonly になる' do
      event_store = create(:event_store)
      expect(event_store.readonly?).to be true
    end

    it '保存前は readonly ではない' do
      event_store = build(:event_store)
      expect(event_store.readonly?).to be false
    end
  end

  describe 'スコープ' do
    let!(:account_event1) { create(:event_store, aggregate_id: 'account-1001', sequence_number: 1) }
    let!(:account_event2) { create(:event_store, :balance_increased, aggregate_id: 'account-1001', sequence_number: 2) }
    let!(:account_event3) { create(:event_store, :balance_decreased, aggregate_id: 'account-1001', sequence_number: 3) }
    let!(:journal_event) { create(:event_store, :journal_entry, sequence_number: 1) }

    describe '.for_aggregate' do
      it '特定 Aggregate のイベントを sequence_number 順で取得できる' do
        events = EventStore.for_aggregate('account-1001')
        expect(events.size).to eq(3)
        expect(events.map(&:sequence_number)).to eq([1, 2, 3])
      end
    end

    describe '.by_type' do
      it '特定のイベントタイプでフィルタリングできる' do
        events = EventStore.by_type('AccountCreatedEvent')
        expect(events.size).to eq(1)
        expect(events.first.event_type).to eq('AccountCreatedEvent')
      end
    end

    describe '.by_correlation' do
      let(:correlation_id) { SecureRandom.uuid }
      let!(:correlated_event1) do
        create(:event_store, aggregate_id: 'account-2001', correlation_id: correlation_id, sequence_number: 1)
      end
      let!(:correlated_event2) do
        create(:event_store, aggregate_id: 'account-2002', correlation_id: correlation_id, sequence_number: 1)
      end

      it '同じ correlation_id のイベントを取得できる' do
        events = EventStore.by_correlation(correlation_id)
        expect(events.size).to eq(2)
      end
    end

    describe '.since / .until' do
      let(:base_time) { Time.zone.parse('2024-01-15 10:00:00') }

      before do
        EventStore.delete_all
        create(:event_store, aggregate_id: 'account-3001', occurred_at: base_time - 2.hours, sequence_number: 1)
        create(:event_store, aggregate_id: 'account-3002', occurred_at: base_time, sequence_number: 1)
        create(:event_store, aggregate_id: 'account-3003', occurred_at: base_time + 2.hours, sequence_number: 1)
      end

      it '特定時刻以降のイベントを取得できる' do
        events = EventStore.since(base_time)
        expect(events.size).to eq(2)
      end

      it '特定時刻以前のイベントを取得できる' do
        events = EventStore.until(base_time)
        expect(events.size).to eq(2)
      end
    end
  end

  describe 'クラスメソッド' do
    describe '.latest_sequence_for' do
      it 'Aggregate の最新シーケンス番号を取得できる' do
        create(:event_store, aggregate_id: 'account-4001', sequence_number: 1)
        create(:event_store, aggregate_id: 'account-4001', event_type: 'BalanceIncreasedEvent', sequence_number: 2)
        create(:event_store, aggregate_id: 'account-4001', event_type: 'BalanceDecreasedEvent', sequence_number: 3)

        expect(EventStore.latest_sequence_for('account-4001')).to eq(3)
      end

      it 'イベントが存在しない場合は 0 を返す' do
        expect(EventStore.latest_sequence_for('nonexistent')).to eq(0)
      end
    end

    describe '.events_for' do
      it 'Aggregate のすべてのイベントを取得できる' do
        create(:event_store, aggregate_id: 'account-5001', sequence_number: 1)
        create(:event_store, aggregate_id: 'account-5001', event_type: 'BalanceIncreasedEvent', sequence_number: 2)

        events = EventStore.events_for('account-5001')
        expect(events.size).to eq(2)
      end
    end

    describe '.events_until' do
      let(:base_time) { Time.zone.parse('2024-01-15 10:00:00') }

      it '特定時点までのイベントを取得できる' do
        create(:event_store, aggregate_id: 'account-6001', occurred_at: base_time - 1.hour, sequence_number: 1)
        create(:event_store, aggregate_id: 'account-6001', occurred_at: base_time, event_type: 'BalanceIncreasedEvent',
                             sequence_number: 2)
        create(:event_store, aggregate_id: 'account-6001', occurred_at: base_time + 1.hour,
                             event_type: 'BalanceDecreasedEvent', sequence_number: 3)

        events = EventStore.events_until('account-6001', base_time)
        expect(events.size).to eq(2)
      end
    end

    describe '.save_event' do
      it 'イベントを保存できる' do
        event = EventStore.save_event(
          aggregate_id: 'account-7001',
          aggregate_type: 'Account',
          event_type: 'AccountCreatedEvent',
          event_version: 1,
          event_data: { account_id: '7001', name: '現金' },
          sequence_number: 1,
          user_id: 'user-123'
        )

        expect(event).to be_persisted
        expect(event.aggregate_id).to eq('account-7001')
      end

      it '同じ sequence_number で保存しようとすると ConcurrentModificationError が発生する' do
        EventStore.save_event(
          aggregate_id: 'account-8001',
          aggregate_type: 'Account',
          event_type: 'AccountCreatedEvent',
          event_version: 1,
          event_data: { account_id: '8001', name: '現金' },
          sequence_number: 1,
          user_id: 'user-123'
        )

        expect do
          EventStore.save_event(
            aggregate_id: 'account-8001',
            aggregate_type: 'Account',
            event_type: 'BalanceIncreasedEvent',
            event_version: 1,
            event_data: { account_id: '8001', amount: 100_000 },
            sequence_number: 1, # 同じシーケンス番号
            user_id: 'user-123'
          )
        end.to raise_error(EventStore::ConcurrentModificationError)
      end
    end
  end
end
