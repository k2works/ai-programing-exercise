# frozen_string_literal: true

require 'rails_helper'

RSpec.describe JournalEntry, type: :model do
  describe 'Red - 仕訳エントリのCRUD操作' do
    before do
      # テスト用勘定科目を作成
      create(:account, code: '1100', name: '現金', account_type: :asset)
      create(:account, code: '1200', name: '普通預金', account_type: :asset)
      create(:account, code: '1300', name: '売掛金', account_type: :asset)
      create(:account, code: '2120', name: '仮受消費税', account_type: :liability)
      create(:account, code: '4100', name: '売上', account_type: :revenue)
      create(:account, code: '6200', name: '支払手数料', account_type: :expense)
    end

    context '仕訳エントリを登録できる' do
      it '現金売上の仕訳を登録できる' do
        # 1. 仕訳エントリを作成
        entry = JournalEntry.create!(
          entry_number: 'JE240001',
          entry_date: Date.new(2024, 1, 15),
          description: '現金売上',
          total_amount: 110_000,
          created_by: 'user001'
        )

        expect(entry).to be_persisted
        expect(entry.entry_number).to eq('JE240001')

        # 2. 仕訳明細を作成（借方：現金、貸方：売上+消費税）
        # 借方：現金 110,000
        entry.details.create!(
          line_number: 1,
          account_code: '1100',
          debit_amount: 110_000,
          credit_amount: 0,
          description: '商品売上による現金収入'
        )

        # 貸方：売上 100,000
        entry.details.create!(
          line_number: 2,
          account_code: '4100',
          debit_amount: 0,
          credit_amount: 100_000,
          description: '商品売上'
        )

        # 貸方：仮受消費税 10,000
        entry.details.create!(
          line_number: 3,
          account_code: '2120',
          debit_amount: 0,
          credit_amount: 10_000,
          description: '消費税'
        )

        # 3. 借方・貸方の合計を検証（複式簿記の原理）
        debit_total = entry.details.sum(:debit_amount)
        credit_total = entry.details.sum(:credit_amount)

        # 複式簿記の原理：借方合計 = 貸方合計
        expect(debit_total).to eq(credit_total)
        expect(debit_total).to eq(110_000)
      end
    end

    context '仕訳エントリを更新できる' do
      it '摘要を更新できる' do
        entry = create(:journal_entry,
                       entry_number: 'JE240001',
                       entry_date: Date.new(2024, 1, 15),
                       description: '現金売上',
                       total_amount: 110_000,
                       created_by: 'user001')

        entry.update!(
          description: '現金売上（修正）',
          updated_by: 'user002'
        )

        expect(entry.reload.description).to eq('現金売上（修正）')
        expect(entry.updated_by).to eq('user002')
      end
    end

    context '仕訳エントリを削除できる（明細も連鎖削除）' do
      it '仕訳エントリと明細が削除される' do
        entry = create(:journal_entry, :with_details)

        expect do
          entry.destroy!
        end.to change(JournalEntry, :count).by(-1)
                                            .and change(JournalEntryDetail, :count).by(-2) # 借方・貸方の2明細
      end
    end

    context '複雑な仕訳エントリ（売掛金回収と振込手数料）を登録できる' do
      it '複数明細行の仕訳を登録できる' do
        entry = JournalEntry.create!(
          entry_number: 'JE240002',
          entry_date: Date.new(2024, 1, 20),
          description: '売掛金回収と振込手数料',
          total_amount: 105_000,
          created_by: 'user001'
        )

        # 借方：普通預金 104,500（振込手数料差引後）
        entry.details.create!(
          line_number: 1,
          account_code: '1200',
          debit_amount: 104_500,
          credit_amount: 0,
          description: '売掛金回収（振込手数料差引後）'
        )

        # 借方：支払手数料 500
        entry.details.create!(
          line_number: 2,
          account_code: '6200',
          debit_amount: 500,
          credit_amount: 0,
          description: '振込手数料'
        )

        # 貸方：売掛金 105,000
        entry.details.create!(
          line_number: 3,
          account_code: '1300',
          debit_amount: 0,
          credit_amount: 105_000,
          description: '売掛金回収'
        )

        # 借方・貸方の合計を検証
        debit_total = entry.details.sum(:debit_amount)
        credit_total = entry.details.sum(:credit_amount)

        expect(debit_total).to eq(credit_total)
        expect(debit_total).to eq(105_000)
      end
    end
  end
end
