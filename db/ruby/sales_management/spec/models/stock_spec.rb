# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Stock, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:lot_number) }
    it { is_expected.to validate_presence_of(:stock_type) }
    it { is_expected.to validate_presence_of(:quality_type) }
    it { is_expected.to validate_numericality_of(:actual_quantity).is_greater_than_or_equal_to(0) }
    it { is_expected.to validate_numericality_of(:valid_quantity).is_greater_than_or_equal_to(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:warehouse) }
    it { is_expected.to belong_to(:product) }
  end

  describe '複合主キー' do
    let(:warehouse) { create(:warehouse) }
    let(:product) { create(:product) }

    it '同じ倉庫・商品でもロットが異なれば別在庫として登録できる' do
      create(:stock, warehouse: warehouse, product: product, lot_number: 'LOT001')
      create(:stock, warehouse: warehouse, product: product, lot_number: 'LOT002')

      expect(described_class.count).to eq(2)
    end

    it '5つのキーが同じ場合は一意制約違反となる' do
      create(:stock,
             warehouse: warehouse,
             product: product,
             lot_number: 'LOT001',
             stock_type: 'normal',
             quality_type: 'good')

      expect do
        create(:stock,
               warehouse: warehouse,
               product: product,
               lot_number: 'LOT001',
               stock_type: 'normal',
               quality_type: 'good')
      end.to raise_error(ActiveRecord::RecordNotUnique)
    end
  end

  describe 'メソッド' do
    it '引当数量を計算できる' do
      stock = create(:stock, actual_quantity: 100, valid_quantity: 80)
      expect(stock.allocated_quantity).to eq(20)
    end
  end
end
