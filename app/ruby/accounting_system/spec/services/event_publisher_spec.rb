# frozen_string_literal: true

require 'rails_helper'

RSpec.describe EventPublisher do
  let(:publisher) { described_class.new }
  let(:event) do
    DomainEvents::JournalEntryCreatedEvent.new(
      journal_entry_id: 'journal-123',
      entry_date: Date.parse('2024-01-15'),
      description: '売上計上',
      line_items: [
        { account_code: '1001', debit_or_credit: 'D', amount: 100_000 },
        { account_code: '4001', debit_or_credit: 'C', amount: 100_000 }
      ],
      user_id: 'user-001',
      occurred_at: Time.current
    )
  end

  describe '#publish_journal_entry_created' do
    context 'RabbitMQが無効な場合（テスト環境）' do
      it 'イベントをパブリッシュせずにリターンする' do
        expect { publisher.publish_journal_entry_created(event) }.not_to raise_error
      end
    end

    context 'RabbitMQが有効な場合' do
      before do
        allow(RabbitMQConfig).to receive(:enabled?).and_return(true)
        @mock_exchange = instance_double(Bunny::Exchange)
        allow(RabbitMQConfig).to receive(:exchange).and_return(@mock_exchange)
      end

      it 'イベントを正しいルーティングキーでパブリッシュする' do
        expect(@mock_exchange).to receive(:publish).with(
          event.to_json,
          hash_including(
            routing_key: RabbitMQConfig::ROUTING_KEY_JOURNAL_CREATED,
            persistent: true,
            content_type: 'application/json'
          )
        )

        publisher_with_mock = described_class.new
        publisher_with_mock.publish_journal_entry_created(event)
      end
    end
  end

  describe '#publish_journal_entry_approved' do
    let(:approved_event) do
      DomainEvents::JournalEntryApprovedEvent.new(
        journal_entry_id: 'journal-123',
        approved_by: 'manager-001',
        approval_comment: '承認します',
        occurred_at: Time.current
      )
    end

    context 'RabbitMQが無効な場合（テスト環境）' do
      it 'イベントをパブリッシュせずにリターンする' do
        expect { publisher.publish_journal_entry_approved(approved_event) }.not_to raise_error
      end
    end
  end

  describe '#publish_journal_entry_deleted' do
    let(:deleted_event) do
      DomainEvents::JournalEntryDeletedEvent.new(
        journal_entry_id: 'journal-123',
        deleted_by: 'admin-001',
        reason: '誤入力のため削除',
        occurred_at: Time.current
      )
    end

    context 'RabbitMQが無効な場合（テスト環境）' do
      it 'イベントをパブリッシュせずにリターンする' do
        expect { publisher.publish_journal_entry_deleted(deleted_event) }.not_to raise_error
      end
    end
  end
end
