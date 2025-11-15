# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Invoice, type: :model do
  subject { build(:invoice) }

  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:invoice_number) }
    it { is_expected.to validate_presence_of(:invoice_date) }
    it { is_expected.to validate_presence_of(:closing_date) }
    it { is_expected.to validate_presence_of(:due_date) }
    it { is_expected.to validate_uniqueness_of(:invoice_number) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
    it { is_expected.to have_many(:invoice_items).dependent(:destroy) }
    it { is_expected.to have_many(:payments).dependent(:restrict_with_error) }
  end

  describe 'メソッド' do
    let(:invoice) { create(:invoice) }
    let!(:item1) { create(:invoice_item, invoice: invoice, amount: 10_000) }
    let!(:item2) { create(:invoice_item, invoice: invoice, amount: 5000) }

    it '合計金額を計算できる' do
      expect(invoice.total_amount).to eq(15_000)
    end

    it '入金額を計算できる' do
      create(:payment, invoice: invoice, amount: 10_000)
      create(:payment, invoice: invoice, amount: 3000)

      expect(invoice.paid_amount).to eq(13_000)
    end

    it '残高を計算できる' do
      create(:payment, invoice: invoice, amount: 10_000)

      expect(invoice.balance).to eq(5000)
    end

    it '完済済みかチェックできる' do
      expect(invoice.fully_paid?).to be false

      create(:payment, invoice: invoice, amount: 15_000)
      expect(invoice.reload.fully_paid?).to be true
    end
  end
end
