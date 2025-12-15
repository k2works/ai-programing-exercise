# frozen_string_literal: true

require 'rails_helper'

RSpec.describe JournalEntryRepository, type: :repository do
  let(:repository) { described_class.new }
  let(:cash_account) { create(:cash_account) }
  let(:sales_account) { create(:account, :revenue, code: '4010', name: '売上高') }

  describe '#find_all' do
    it 'すべての仕訳を日付順で取得できる' do
      # Given: 複数の仕訳を作成
      entry1 = create_journal_entry(date: Date.new(2025, 1, 15))
      entry2 = create_journal_entry(date: Date.new(2025, 1, 10))

      # When: すべての仕訳を取得
      entries = repository.find_all

      # Then: 日付降順で取得できる
      expect(entries).to be_a(Array)
      expect(entries.length).to eq(2)
      expect(entries[0].id).to eq(entry1.id)
      expect(entries[1].id).to eq(entry2.id)
    end

    it '仕訳が存在しない場合は空配列を返す' do
      # When: 仕訳が存在しない状態で取得
      entries = repository.find_all

      # Then: 空配列を返す
      expect(entries).to eq([])
    end
  end

  describe '#find_by_id' do
    it 'IDで仕訳を取得できる' do
      # Given: 仕訳を作成
      entry = create_journal_entry

      # When: IDで検索
      found = repository.find_by_id(entry.id)

      # Then: 仕訳を取得できる
      expect(found).not_to be_nil
      expect(found.id).to eq(entry.id)
      expect(found.entry_number).to eq(entry.entry_number)
    end

    it '存在しないIDの場合はnilを返す' do
      # When: 存在しないIDで検索
      found = repository.find_by_id(9999)

      # Then: nilを返す
      expect(found).to be_nil
    end
  end

  describe '#save' do
    it '新しい仕訳を作成できる' do
      # Given: 新しい仕訳
      entry = JournalEntry.new(
        entry_number: 'J999',
        entry_date: Date.new(2025, 1, 15),
        description: 'テスト仕訳',
        total_amount: 10_000,
        created_by: 'test_user'
      )
      entry.details.build(
        line_number: 1,
        account_code: cash_account.code,
        debit_amount: 10_000,
        credit_amount: 0,
        description: '現金'
      )
      entry.details.build(
        line_number: 2,
        account_code: sales_account.code,
        debit_amount: 0,
        credit_amount: 10_000,
        description: '売上高'
      )

      # When: 保存
      saved_entry = repository.save(entry)

      # Then: 保存される
      expect(saved_entry).to be_persisted
      expect(saved_entry.entry_number).to eq('J999')
      expect(saved_entry.details.count).to eq(2)
      expect(JournalEntry.find_by(entry_number: 'J999')).not_to be_nil
    end

    it '既存の仕訳を更新できる' do
      # Given: 既存の仕訳
      entry = create_journal_entry

      # When: 摘要を更新
      entry.description = '更新されたテスト仕訳'
      saved_entry = repository.save(entry)

      # Then: 更新される
      expect(saved_entry.description).to eq('更新されたテスト仕訳')
      expect(JournalEntry.find(entry.id).description).to eq('更新されたテスト仕訳')
    end
  end

  describe '#delete_by_id' do
    it 'IDで仕訳を削除できる' do
      # Given: 仕訳を作成
      entry = create_journal_entry

      # When: 削除
      repository.delete_by_id(entry.id)

      # Then: 削除される
      expect(JournalEntry.find_by(id: entry.id)).to be_nil
    end

    it '存在しないIDの場合はエラーにならない' do
      # When & Then: 存在しないIDを削除してもエラーにならない
      expect { repository.delete_by_id(9999) }.not_to raise_error
    end
  end

  private

  def create_journal_entry(date: Date.new(2025, 1, 15))
    counter = JournalEntry.count + 1
    JournalEntry.create!(
      entry_number: format('J%03d', counter),
      entry_date: date,
      description: 'テスト仕訳',
      total_amount: 10_000,
      created_by: 'test_user'
    ).tap do |entry|
      entry.details.create!(
        line_number: 1,
        account_code: cash_account.code,
        debit_amount: 10_000,
        credit_amount: 0,
        description: '現金'
      )
      entry.details.create!(
        line_number: 2,
        account_code: sales_account.code,
        debit_amount: 0,
        credit_amount: 10_000,
        description: '売上高'
      )
    end
  end
end
