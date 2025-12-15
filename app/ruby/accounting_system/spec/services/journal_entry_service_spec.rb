# frozen_string_literal: true

require 'rails_helper'
require_relative '../../app/services/account_service'

RSpec.describe JournalEntryService, type: :service do
  let(:journal_repository) { instance_double(JournalEntryRepository) }
  let(:account_repository) { instance_double(AccountRepository) }
  let(:service) { described_class.new(journal_entry_repository: journal_repository, account_repository: account_repository) }

  let(:cash_account) { Account.new(code: '1010', name: '現金') }
  let(:sales_account) { Account.new(code: '4010', name: '売上高') }

  describe '#all_journal_entries' do
    it 'すべての仕訳を取得できる' do
      # Given: リポジトリから仕訳リストを返す
      entries = [
        JournalEntry.new(id: 1, entry_number: 'J001'),
        JournalEntry.new(id: 2, entry_number: 'J002')
      ]
      allow(journal_repository).to receive(:find_all).and_return(entries)

      # When: すべての仕訳を取得
      result = service.all_journal_entries

      # Then: リポジトリから取得した仕訳を返す
      expect(result).to eq(entries)
      expect(journal_repository).to have_received(:find_all)
    end
  end

  describe '#find_journal_entry' do
    it 'IDで仕訳を取得できる' do
      # Given: リポジトリから仕訳を返す
      entry = JournalEntry.new(id: 1, entry_number: 'J001')
      allow(journal_repository).to receive(:find_by_id).with(1).and_return(entry)

      # When: IDで検索
      result = service.find_journal_entry(1)

      # Then: 仕訳を返す
      expect(result).to eq(entry)
      expect(journal_repository).to have_received(:find_by_id).with(1)
    end

    it '仕訳が見つからない場合はエラーを発生させる' do
      # Given: リポジトリからnilを返す
      allow(journal_repository).to receive(:find_by_id).with(999).and_return(nil)

      # When & Then: エラーを発生させる
      expect { service.find_journal_entry(999) }.to raise_error(JournalEntryNotFoundError, '仕訳ID 999 が見つかりません')
    end
  end

  describe '#create_journal_entry' do
    it '正しい仕訳を作成できる' do
      # Given: 正しい仕訳データ
      entry = JournalEntry.new(
        entry_number: 'J001',
        entry_date: Date.new(2025, 1, 15),
        description: 'テスト仕訳',
        total_amount: 10_000,
        created_by: 'test_user'
      )
      entry.details.build(
        line_number: 1,
        account_code: '1010',
        debit_amount: 10_000,
        credit_amount: 0,
        description: '現金'
      )
      entry.details.build(
        line_number: 2,
        account_code: '4010',
        debit_amount: 0,
        credit_amount: 10_000,
        description: '売上高'
      )

      # モック設定
      allow(account_repository).to receive(:find_by_code).with('1010').and_return(cash_account)
      allow(account_repository).to receive(:find_by_code).with('4010').and_return(sales_account)
      allow(journal_repository).to receive(:save) do |journal|
        journal.id = 1
        journal
      end

      # When: 仕訳を作成
      attributes = {
        entry_number: 'J001',
        entry_date: Date.new(2025, 1, 15),
        description: 'テスト仕訳',
        total_amount: 10_000,
        created_by: 'test_user',
        details_attributes: [
          { line_number: 1, account_code: '1010', debit_amount: 10_000, credit_amount: 0, description: '現金' },
          { line_number: 2, account_code: '4010', debit_amount: 0, credit_amount: 10_000, description: '売上高' }
        ]
      }
      result = service.create_journal_entry(attributes)

      # Then: 仕訳が作成される
      expect(result.entry_number).to eq('J001')
      expect(journal_repository).to have_received(:save)
    end

    it '貸借が一致しない場合はエラーを発生させる' do
      # Given: 貸借が一致しない仕訳データ
      attributes = {
        entry_number: 'J001',
        entry_date: Date.new(2025, 1, 15),
        description: 'テスト仕訳',
        total_amount: 10_000,
        created_by: 'test_user',
        details_attributes: [
          { line_number: 1, account_code: '1010', debit_amount: 10_000, credit_amount: 0, description: '現金' },
          { line_number: 2, account_code: '4010', debit_amount: 0, credit_amount: 5_000, description: '売上高' }
        ]
      }

      # モック設定
      allow(account_repository).to receive(:find_by_code).with('1010').and_return(cash_account)
      allow(account_repository).to receive(:find_by_code).with('4010').and_return(sales_account)

      # When & Then: エラーを発生させる
      expect { service.create_journal_entry(attributes) }.to raise_error(InvalidJournalEntryError, '貸借が一致していません')
    end

    it '勘定科目が存在しない場合はエラーを発生させる' do
      # Given: 存在しない勘定科目を含む仕訳データ
      attributes = {
        entry_number: 'J001',
        entry_date: Date.new(2025, 1, 15),
        description: 'テスト仕訳',
        total_amount: 10_000,
        created_by: 'test_user',
        details_attributes: [
          { line_number: 1, account_code: '9999', debit_amount: 10_000, credit_amount: 0, description: '不明' },
          { line_number: 2, account_code: '4010', debit_amount: 0, credit_amount: 10_000, description: '売上高' }
        ]
      }

      # モック設定
      allow(account_repository).to receive(:find_by_code).with('9999').and_return(nil)
      allow(account_repository).to receive(:find_by_code).with('4010').and_return(sales_account)

      # When & Then: エラーを発生させる
      expect { service.create_journal_entry(attributes) }.to raise_error(AccountNotFoundError, '勘定科目が存在しません: 9999')
    end

    it '明細が2件未満の場合はエラーを発生させる' do
      # Given: 明細が1件の仕訳データ
      attributes = {
        entry_number: 'J001',
        entry_date: Date.new(2025, 1, 15),
        description: 'テスト仕訳',
        total_amount: 10_000,
        created_by: 'test_user',
        details_attributes: [
          { line_number: 1, account_code: '1010', debit_amount: 10_000, credit_amount: 0, description: '現金' }
        ]
      }

      # When & Then: エラーを発生させる
      expect { service.create_journal_entry(attributes) }.to raise_error(InvalidJournalEntryError, '仕訳明細は2件以上必要です')
    end
  end

  describe '#delete_journal_entry' do
    it '仕訳を削除できる' do
      # Given: 既存の仕訳
      entry = JournalEntry.new(id: 1, entry_number: 'J001')
      allow(journal_repository).to receive(:find_by_id).with(1).and_return(entry)
      allow(journal_repository).to receive(:delete_by_id).with(1)

      # When: 仕訳を削除
      service.delete_journal_entry(1)

      # Then: 削除される
      expect(journal_repository).to have_received(:delete_by_id).with(1)
    end

    it '存在しない仕訳を削除しようとするとエラーを発生させる' do
      # Given: 存在しない仕訳
      allow(journal_repository).to receive(:find_by_id).with(999).and_return(nil)

      # When & Then: エラーを発生させる
      expect { service.delete_journal_entry(999) }.to raise_error(JournalEntryNotFoundError)
    end
  end
end
