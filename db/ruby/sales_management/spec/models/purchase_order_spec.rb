# frozen_string_literal: true

require 'rails_helper'

RSpec.describe PurchaseOrder, type: :model do
  subject { build(:purchase_order) }

  describe 'バリデーション' do
    # order_numberは自動生成されるため、presenceバリデーションのテストはスキップ
    it { is_expected.to validate_uniqueness_of(:order_number) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
    it { is_expected.to belong_to(:warehouse) }
    it { is_expected.to belong_to(:sales_order).optional }
    it { is_expected.to have_many(:purchase_order_items).dependent(:destroy) }
    it { is_expected.to have_many(:purchases).dependent(:restrict_with_error) }
  end

  describe 'ステータス管理' do
    let(:purchase_order) { create(:purchase_order) }

    it 'デフォルトステータスはdraft' do
      expect(purchase_order.draft?).to be true
    end

    it 'ステータスを変更できる' do
      purchase_order.submit!
      expect(purchase_order.submitted?).to be true

      purchase_order.receive!
      expect(purchase_order.received?).to be true
    end
  end

  describe '入荷処理' do
    let(:purchase_order) { create(:purchase_order) }
    let!(:item1) { create(:purchase_order_item, purchase_order: purchase_order, quantity: 10) }
    let!(:item2) { create(:purchase_order_item, purchase_order: purchase_order, quantity: 20) }

    it '全明細の入荷完了を確認できる' do
      expect(purchase_order.all_received?).to be false

      item1.update!(received_quantity: 10)
      item2.update!(received_quantity: 20)

      expect(purchase_order.reload.all_received?).to be true
    end
  end
end
