# frozen_string_literal: true

require 'rails_helper'

RSpec.describe ProductCategory, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:code) }
    it { is_expected.to validate_presence_of(:name) }
    it { is_expected.to validate_uniqueness_of(:code) }
  end

  describe '関連' do
    it { is_expected.to have_many(:products).dependent(:destroy) }
  end

  describe '動作確認' do
    it '有効なファクトリを持つこと' do
      expect(build(:product_category)).to be_valid
    end
  end
end
