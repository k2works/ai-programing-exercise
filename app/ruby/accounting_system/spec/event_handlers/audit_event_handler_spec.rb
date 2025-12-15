# frozen_string_literal: true

require 'rails_helper'

RSpec.describe EventHandlers::AuditEventHandler do
  describe '.handle_account_updated' do
    it '勘定科目の更新イベントを監査ログに記録する' do
      payload = {
        account_code: '1001',
        old_values: { account_name: '現金' },
        new_values: { account_name: '現金及び預金' },
        user_id: 'user001',
        user_name: '山田太郎',
        ip_address: '192.168.1.100'
      }

      expect do
        described_class.handle_account_updated(payload)
      end.to have_enqueued_job(AuditLogRecorderJob)
    end
  end

  describe '.handle_account_created' do
    it '勘定科目の作成イベントを監査ログに記録する' do
      payload = {
        account_code: '1001',
        change_data: { account_name: '現金', bspl_type: 'BS', account_type: 'asset' },
        user_id: 'user001',
        user_name: '山田太郎',
        ip_address: '192.168.1.100'
      }

      expect do
        described_class.handle_account_created(payload)
      end.to have_enqueued_job(AuditLogRecorderJob)
    end
  end

  describe '.handle_journal_created' do
    it '仕訳の作成イベントを監査ログに記録する' do
      payload = {
        journal_id: 1,
        journal_date: Date.today,
        description: '売上計上',
        journal_data: { amount: 100_000 },
        user_id: 'user001',
        user_name: '山田太郎',
        ip_address: '192.168.1.100'
      }

      expect do
        described_class.handle_journal_created(payload)
      end.to have_enqueued_job(AuditLogRecorderJob)
    end
  end
end
