# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Account Refactoring', type: :model do
  describe 'BSPL区分の追加' do
    it 'BSPL区分を設定できる' do
      account = Account.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        bspl_type: 'B', # 貸借対照表
        balance: 0
      )

      expect(account.bspl_type).to eq('B')
      expect(account).to be_persisted
    end

    it '損益計算書科目を設定できる' do
      account = Account.create!(
        code: '4000',
        name: '売上高',
        account_type: :revenue,
        bspl_type: 'P', # 損益計算書
        balance: 0
      )

      expect(account.bspl_type).to eq('P')
      expect(account).to be_persisted
    end
  end

  describe '貸借区分の追加' do
    it '借方科目を設定できる' do
      account = Account.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        debit_credit_type: 'D', # 借方
        balance: 0
      )

      expect(account.debit_credit_type).to eq('D')
      expect(account).to be_persisted
    end

    it '貸方科目を設定できる' do
      account = Account.create!(
        code: '2000',
        name: '買掛金',
        account_type: :liability,
        debit_credit_type: 'C', # 貸方
        balance: 0
      )

      expect(account.debit_credit_type).to eq('C')
      expect(account).to be_persisted
    end
  end

  describe '集計科目フラグの追加' do
    it '集計科目を設定できる' do
      account = Account.create!(
        code: '1000',
        name: '流動資産',
        account_type: :asset,
        is_summary: true, # 集計科目フラグ
        balance: 0
      )

      expect(account.is_summary).to be true
      expect(account).to be_persisted
    end

    it '明細科目を設定できる' do
      account = Account.create!(
        code: '1001',
        name: '現金',
        account_type: :asset,
        is_summary: false, # 明細科目
        balance: 0
      )

      expect(account.is_summary).to be false
      expect(account).to be_persisted
    end
  end
end
