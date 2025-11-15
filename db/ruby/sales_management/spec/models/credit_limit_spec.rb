# frozen_string_literal: true

require 'rails_helper'

RSpec.describe CreditLimit, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:limit_amount) }
    it { is_expected.to validate_numericality_of(:limit_amount).is_greater_than_or_equal_to(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
  end

  describe '与信チェック' do
    let(:customer) { create(:party, :organization) }
    let!(:credit_limit) { create(:credit_limit, party: customer, limit_amount: 1_000_000) }

    it '与信限度額内の注文は許可される' do
      order = build(:order, :sales, party: customer)
      order.order_items.build(product: create(:product), quantity: 50, unit_price: 10_000) # 500,000円

      expect(credit_limit.available?(order.calculate_total)).to be true
    end

    it '与信限度額を超える注文は拒否される' do
      order = build(:order, :sales, party: customer)
      order.order_items.build(product: create(:product), quantity: 150, unit_price: 10_000) # 1,500,000円

      expect(credit_limit.available?(order.calculate_total)).to be false
    end

    it '未払残高を考慮して与信チェックできる' do
      # 未払いの請求が500,000円ある
      invoice = create(:invoice, party: customer)
      create(:invoice_item, invoice: invoice, amount: 500_000)

      # 新規注文が600,000円
      order = build(:order, :sales, party: customer)
      order.order_items.build(product: create(:product), quantity: 60, unit_price: 10_000)

      # 合計1,100,000円なので限度額1,000,000円を超える
      expect(credit_limit.available_with_outstanding?(order.calculate_total)).to be false
    end
  end
end
