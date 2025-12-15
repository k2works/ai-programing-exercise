# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Account Tax Code', type: :model do
  describe '課税取引コードの追加' do
    it '課税取引コードを設定できる' do
      account = Account.create!(
        code: '4000',
        name: '売上高',
        account_type: :revenue,
        bspl_type: 'P',
        transaction_type: '4',
        tax_code: '01', # 課税
        balance: 0
      )

      expect(account.tax_code).to eq('01')
      expect(account).to be_persisted
    end

    it '課税取引コードなしで勘定科目を作成できる' do
      account = Account.create!(
        code: '3000',
        name: '資本金',
        account_type: :equity,
        bspl_type: 'B',
        transaction_type: '3',
        balance: 0
      )

      expect(account.tax_code).to be_nil
      expect(account).to be_persisted
    end

    it '課税取引コードは2文字以内' do
      account = build(:account, tax_code: '123')

      expect(account).not_to be_valid
      expect(account.errors[:tax_code]).to be_present
    end
  end
end
