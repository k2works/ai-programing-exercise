# frozen_string_literal: true

require 'rails_helper'

RSpec.describe '3層構造仕訳テーブル', type: :model do
  before do
    # テストデータのクリーンアップ
    JournalDetailItem.delete_all if defined?(JournalDetailItem)
    JournalDetail.delete_all if defined?(JournalDetail)
    Journal.delete_all if defined?(Journal)
  end

  describe '3層構造での仕訳登録' do
    context '単純な仕訳' do
      it '現金100,000円で商品を仕入れる仕訳を登録できる' do
        # Given: 現金100,000円で商品を仕入れる仕訳
        journal_no = 'JE-20250101-001'
        journal_date = Date.new(2025, 1, 1)
        input_date = Date.new(2025, 1, 1)

        # When: 仕訳を登録
        # 1. 仕訳ヘッダー
        journal = Journal.create!(
          journal_no: journal_no,
          journal_date: journal_date,
          input_date: input_date,
          settlement_flag: 0,
          single_entry_flag: 1,
          slip_type: 0,
          periodic_flag: 0,
          red_slip_flag: 0
        )

        # 2. 仕訳明細（1行）
        detail = JournalDetail.create!(
          journal: journal,
          line_number: 1,
          description: '商品仕入'
        )

        # 3. 仕訳貸借明細（借方：仕入、貸方：現金）
        JournalDetailItem.create!(
          journal_detail: detail,
          debit_credit_type: 'D',
          currency_code: 'JPY',
          exchange_rate: 1.00,
          account_code: '5110',
          amount: 100_000.00,
          base_amount: 100_000.00,
          cash_flow_flag: 0
        )

        JournalDetailItem.create!(
          journal_detail: detail,
          debit_credit_type: 'C',
          currency_code: 'JPY',
          exchange_rate: 1.00,
          account_code: '1010',
          amount: 100_000.00,
          base_amount: 100_000.00,
          cash_flow_flag: 0
        )

        # Then: データが正しく登録されていることを確認
        # 1. 仕訳が登録されている
        expect(Journal.where(journal_no: journal_no).count).to eq(1)

        # 2. 仕訳明細が登録されている
        expect(JournalDetail.where(journal_id: journal.id).count).to eq(1)

        # 3. 仕訳貸借明細が2件（借方・貸方）登録されている
        expect(JournalDetailItem.joins(:journal_detail)
                                 .where(journal_details: { journal_id: journal.id })
                                 .count).to eq(2)

        # 4. 借方・貸方の合計が一致する（複式簿記の原理）
        items = JournalDetailItem.joins(:journal_detail)
                                  .where(journal_details: { journal_id: journal.id })

        debit_total = items.where(debit_credit_type: 'D').sum(:amount)
        credit_total = items.where(debit_credit_type: 'C').sum(:amount)

        expect(debit_total).to eq(credit_total)
        expect(debit_total).to eq(100_000.00)
      end
    end

    context '複合仕訳' do
      it '売掛金の回収（振込手数料差引）を登録できる' do
        # Given: 売掛金 100,000円 → 普通預金 99,560円 + 支払手数料 440円
        journal_no = 'JE-20250102-001'
        journal_date = Date.new(2025, 1, 2)
        input_date = Date.new(2025, 1, 2)

        # When: 仕訳を登録
        # 1. 仕訳ヘッダー
        journal = Journal.create!(
          journal_no: journal_no,
          journal_date: journal_date,
          input_date: input_date,
          settlement_flag: 0,
          single_entry_flag: 0, # 複合仕訳
          slip_type: 0,
          periodic_flag: 0,
          red_slip_flag: 0
        )

        # 2. 仕訳明細（2行）
        detail1 = JournalDetail.create!(
          journal: journal,
          line_number: 1,
          description: '売掛金回収（A社）'
        )

        detail2 = JournalDetail.create!(
          journal: journal,
          line_number: 2,
          description: '振込手数料'
        )

        # 3. 仕訳貸借明細
        # 行1-借方: 普通預金 99,560円
        JournalDetailItem.create!(
          journal_detail: detail1,
          debit_credit_type: 'D',
          currency_code: 'JPY',
          exchange_rate: 1.00,
          account_code: '1020',
          amount: 99_560.00,
          base_amount: 99_560.00,
          cash_flow_flag: 1
        )

        # 行1-貸方: 売掛金 99,560円
        JournalDetailItem.create!(
          journal_detail: detail1,
          debit_credit_type: 'C',
          currency_code: 'JPY',
          exchange_rate: 1.00,
          account_code: '1130',
          amount: 99_560.00,
          base_amount: 99_560.00,
          cash_flow_flag: 0
        )

        # 行2-借方: 支払手数料 440円
        JournalDetailItem.create!(
          journal_detail: detail2,
          debit_credit_type: 'D',
          currency_code: 'JPY',
          exchange_rate: 1.00,
          account_code: '5410',
          amount: 440.00,
          base_amount: 440.00,
          cash_flow_flag: 0
        )

        # 行2-貸方: 売掛金 440円
        JournalDetailItem.create!(
          journal_detail: detail2,
          debit_credit_type: 'C',
          currency_code: 'JPY',
          exchange_rate: 1.00,
          account_code: '1130',
          amount: 440.00,
          base_amount: 440.00,
          cash_flow_flag: 0
        )

        # Then: データが正しく登録されていることを確認
        # 1. 仕訳明細が2件登録されている
        expect(JournalDetail.where(journal_id: journal.id).count).to eq(2)

        # 2. 仕訳貸借明細が4件登録されている
        expect(JournalDetailItem.joins(:journal_detail)
                                 .where(journal_details: { journal_id: journal.id })
                                 .count).to eq(4)

        # 3. 借方・貸方の合計が一致する
        items = JournalDetailItem.joins(:journal_detail)
                                  .where(journal_details: { journal_id: journal.id })

        debit_total = items.where(debit_credit_type: 'D').sum(:amount)
        credit_total = items.where(debit_credit_type: 'C').sum(:amount)

        expect(debit_total).to eq(credit_total)
        expect(debit_total).to eq(100_000.00)

        # 4. 単振フラグが0（複合仕訳）になっている
        expect(journal.single_entry_flag).to eq(0)
      end
    end
  end

  describe '外部キー制約' do
    it '仕訳削除時に明細も削除される（CASCADE DELETE）' do
      # Given: 仕訳を登録
      journal = Journal.create!(
        journal_no: 'JE-20250103-001',
        journal_date: Date.new(2025, 1, 3),
        input_date: Date.new(2025, 1, 3),
        settlement_flag: 0,
        single_entry_flag: 1,
        slip_type: 0,
        periodic_flag: 0,
        red_slip_flag: 0
      )

      detail = JournalDetail.create!(
        journal: journal,
        line_number: 1,
        description: 'テスト'
      )

      JournalDetailItem.create!(
        journal_detail: detail,
        debit_credit_type: 'D',
        currency_code: 'JPY',
        exchange_rate: 1.00,
        account_code: '1010',
        amount: 10_000.00,
        base_amount: 10_000.00,
        cash_flow_flag: 0
      )

      # When: 仕訳を削除
      journal.destroy

      # Then: 明細と貸借明細も自動削除される（dependent: :destroy）
      expect(JournalDetail.where(id: detail.id).count).to eq(0)
      expect(JournalDetailItem.where(journal_detail_id: detail.id).count).to eq(0)
    end
  end
end
