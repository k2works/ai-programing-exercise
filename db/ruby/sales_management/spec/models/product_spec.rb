# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Product, type: :model do
  subject { build(:product) }

  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:code) }
    it { is_expected.to validate_presence_of(:name) }
    it { is_expected.to validate_presence_of(:unit_price) }
    it { is_expected.to validate_uniqueness_of(:code) }
    it { is_expected.to validate_numericality_of(:unit_price).is_greater_than_or_equal_to(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:product_category) }
  end

  describe '動作確認' do
    it '有効なファクトリを持つこと' do
      expect(build(:product)).to be_valid
    end
  end
end
