# frozen_string_literal: true

require 'rails_helper'

RSpec.describe OrderItem, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:quantity) }
    it { is_expected.to validate_presence_of(:unit_price) }
    it { is_expected.to validate_numericality_of(:quantity).is_greater_than(0) }
    it { is_expected.to validate_numericality_of(:unit_price).is_greater_than_or_equal_to(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:order) }
    it { is_expected.to belong_to(:product) }
  end

  describe 'メソッド' do
    it '小計を計算できる' do
      item = build(:order_item, quantity: 5, unit_price: 200)
      expect(item.subtotal).to eq(1000)
    end
  end
end
