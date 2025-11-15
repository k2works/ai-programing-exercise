# frozen_string_literal: true

require 'rails_helper'

RSpec.describe BillPayment, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:payment_date) }
    it { is_expected.to validate_presence_of(:amount) }
    it { is_expected.to validate_numericality_of(:amount).is_greater_than(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:bill) }
  end

  describe 'カスタムバリデーション' do
    it '支払額を超える出金はエラー' do
      bill = create(:bill)
      create(:bill_item, bill: bill, amount: 20_000)

      bill_payment = build(:bill_payment, bill: bill, amount: 25_000)
      expect(bill_payment).not_to be_valid
      expect(bill_payment.errors[:amount]).to include('が支払残高を超えています')
    end

    it '残高内の出金は有効' do
      bill = create(:bill)
      create(:bill_item, bill: bill, amount: 20_000)

      bill_payment = build(:bill_payment, bill: bill, amount: 10_000)
      expect(bill_payment).to be_valid
    end
  end
end
