# frozen_string_literal: true

require 'rails_helper'

RSpec.describe AuditLog, type: :model do
  describe '監査ログの作成' do
    it '監査ログを作成できる' do
      change_data = {
        'journal_date' => '2025-01-15',
        'description' => '売上計上',
        'amount' => 100_000
      }

      audit_log = described_class.create_log(
        entity_type: 'Journal',
        entity_id: '12345',
        action: :create,
        user_id: 'user001',
        user_name: '山田太郎',
        change_data: change_data,
        ip_address: '192.168.1.100'
      )

      expect(audit_log.entity_type).to eq('Journal')
      expect(audit_log.entity_id).to eq('12345')
      expect(audit_log.action).to eq('create')
      expect(audit_log.user_id).to eq('user001')
      expect(audit_log.timestamp).not_to be_nil
    end
  end

  describe '更新操作の記録' do
    it '変更前後の値を記録する' do
      old_values = { 'account_name' => '現金' }
      new_values = { 'account_name' => '現金及び預金' }

      audit_log = described_class.create_for_update(
        entity_type: 'Account',
        entity_id: '1001',
        user_id: 'user001',
        user_name: '山田太郎',
        old_values: old_values,
        new_values: new_values,
        ip_address: '192.168.1.100'
      )

      expect(audit_log.action).to eq('update')
      expect(audit_log.old_values).to eq(old_values)
      expect(audit_log.new_values).to eq(new_values)
    end
  end

  describe '削除操作の記録' do
    it '理由を記録できる' do
      audit_log = described_class.create_for_delete(
        entity_type: 'Journal',
        entity_id: '12345',
        user_id: 'user001',
        user_name: '山田太郎',
        old_values: { 'description' => '売上計上' },
        reason: '入力ミスによる削除',
        ip_address: '192.168.1.100'
      )

      expect(audit_log.action).to eq('delete')
      expect(audit_log.reason).to eq('入力ミスによる削除')
    end
  end

  describe 'タイムスタンプの自動設定' do
    it 'タイムスタンプが自動設定される' do
      before = Time.current

      audit_log = described_class.create_log(
        entity_type: 'Journal',
        entity_id: '1',
        action: :create,
        user_id: 'user001',
        user_name: '山田太郎',
        change_data: {},
        ip_address: nil
      )

      after = Time.current

      expect(audit_log.timestamp).to be_between(before, after)
    end
  end

  describe '不変性' do
    it '監査ログは読み取り専用である' do
      audit_log = create(:audit_log)

      expect do
        audit_log.update(user_name: '変更')
      end.to raise_error(ActiveRecord::ReadOnlyRecord)
    end
  end

  describe 'スコープ' do
    let!(:account_log) { create(:audit_log, entity_type: 'Account', entity_id: '1001') }
    let!(:journal_log) { create(:audit_log, entity_type: 'Journal', entity_id: '12345') }

    it 'エンティティで絞り込める' do
      logs = described_class.by_entity('Account', '1001')
      expect(logs).to include(account_log)
      expect(logs).not_to include(journal_log)
    end

    it 'ユーザーで絞り込める' do
      user_log = create(:audit_log, user_id: 'user001')
      other_log = create(:audit_log, user_id: 'user002')

      logs = described_class.by_user('user001')
      expect(logs).to include(user_log)
      expect(logs).not_to include(other_log)
    end

    it '期間で絞り込める' do
      old_log = create(:audit_log, timestamp: 1.month.ago)
      recent_log = create(:audit_log, timestamp: 1.day.ago)

      logs = described_class.by_period(2.days.ago, Time.current)
      expect(logs).to include(recent_log)
      expect(logs).not_to include(old_log)
    end
  end
end
