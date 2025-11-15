# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Payment, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:payment_date) }
    it { is_expected.to validate_presence_of(:amount) }
    it { is_expected.to validate_numericality_of(:amount).is_greater_than(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:invoice) }
  end

  describe 'カスタムバリデーション' do
    it '請求額を超える入金はエラー' do
      invoice = create(:invoice)
      create(:invoice_item, invoice: invoice, amount: 10_000)

      payment = build(:payment, invoice: invoice, amount: 15_000)
      expect(payment).not_to be_valid
      expect(payment.errors[:amount]).to include('が請求残高を超えています')
    end

    it '残高内の入金は有効' do
      invoice = create(:invoice)
      create(:invoice_item, invoice: invoice, amount: 10_000)

      payment = build(:payment, invoice: invoice, amount: 5000)
      expect(payment).to be_valid
    end
  end
end
