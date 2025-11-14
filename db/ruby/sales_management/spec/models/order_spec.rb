# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Order, type: :model do
  subject { build(:order) }

  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:order_number) }
    it { is_expected.to validate_presence_of(:order_type) }
    it { is_expected.to validate_uniqueness_of(:order_number) }
    it { is_expected.to validate_inclusion_of(:order_type).in_array(%w[Sales Purchase]) }
    it { is_expected.to validate_inclusion_of(:status).in_array(%w[draft confirmed completed cancelled]) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
    it { is_expected.to have_many(:order_items).dependent(:destroy) }
  end

  describe 'スコープ' do
    let!(:sales_order) { create(:order, :sales) }
    let!(:purchase_order) { create(:order, :purchase) }
    let!(:confirmed_order) { create(:order, :sales, :confirmed) }

    it '受注のみ取得できる' do
      expect(described_class.sales).to include(sales_order)
      expect(described_class.sales).not_to include(purchase_order)
    end

    it '発注のみ取得できる' do
      expect(described_class.purchases).to include(purchase_order)
      expect(described_class.purchases).not_to include(sales_order)
    end

    it 'ステータスでフィルタできる' do
      expect(described_class.by_status('confirmed')).to include(confirmed_order)
    end
  end

  describe 'メソッド' do
    it '合計金額を計算できる' do
      order = create(:order, :sales)
      create(:order_item, order: order, quantity: 2, unit_price: 1000)
      create(:order_item, order: order, quantity: 3, unit_price: 500)

      expect(order.total_amount).to eq(3500)
    end

    it 'ステータスを変更できる' do
      order = create(:order, :sales)
      expect(order.draft?).to be true

      order.confirm!
      expect(order.confirmed?).to be true

      order.complete!
      expect(order.completed?).to be true
    end
  end
end
