# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Bill, type: :model do
  subject { build(:bill) }

  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:bill_number) }
    it { is_expected.to validate_presence_of(:bill_date) }
    it { is_expected.to validate_presence_of(:closing_date) }
    it { is_expected.to validate_presence_of(:due_date) }
    it { is_expected.to validate_uniqueness_of(:bill_number) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
    it { is_expected.to have_many(:bill_items).dependent(:destroy) }
    it { is_expected.to have_many(:bill_payments).dependent(:restrict_with_error) }
  end

  describe 'メソッド' do
    let(:bill) { create(:bill) }
    let!(:item1) { create(:bill_item, bill: bill, amount: 20_000) }
    let!(:item2) { create(:bill_item, bill: bill, amount: 8000) }

    it '合計金額を計算できる' do
      expect(bill.total_amount).to eq(28_000)
    end

    it '支払額を計算できる' do
      create(:bill_payment, bill: bill, amount: 20_000)
      expect(bill.paid_amount).to eq(20_000)
    end

    it '残高を計算できる' do
      create(:bill_payment, bill: bill, amount: 15_000)
      expect(bill.balance).to eq(13_000)
    end

    it '完済済みかチェックできる' do
      expect(bill.fully_paid?).to be false

      create(:bill_payment, bill: bill, amount: 28_000)
      expect(bill.reload.fully_paid?).to be true
    end
  end
end
