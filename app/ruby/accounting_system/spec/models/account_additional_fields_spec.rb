# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Account Additional Fields', type: :model do
  describe '取引要素区分の追加' do
    it '資産科目に取引要素区分1を設定できる' do
      account = Account.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        bspl_type: 'B',
        transaction_type: '1',
        balance: 0
      )

      expect(account.transaction_type).to eq('1')
      expect(account).to be_persisted
    end

    it '負債科目に取引要素区分2を設定できる' do
      account = Account.create!(
        code: '2000',
        name: '買掛金',
        account_type: :liability,
        bspl_type: 'B',
        transaction_type: '2',
        balance: 0
      )

      expect(account.transaction_type).to eq('2')
      expect(account).to be_persisted
    end
  end

  describe '費用区分の追加' do
    it '費用科目に費用区分を設定できる' do
      account = Account.create!(
        code: '5000',
        name: '給料',
        account_type: :expense,
        bspl_type: 'P',
        transaction_type: '5',
        expense_type: '1',
        balance: 0
      )

      expect(account.expense_type).to eq('1')
      expect(account).to be_persisted
    end
  end

  describe '表示順序の追加' do
    it '表示順序を設定できる' do
      account = Account.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        bspl_type: 'B',
        transaction_type: '1',
        display_order: 100,
        balance: 0
      )

      expect(account.display_order).to eq(100)
      expect(account).to be_persisted
    end
  end
end
