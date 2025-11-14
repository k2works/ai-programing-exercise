# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Sample, type: :model do
  describe 'バリデーション' do
    it '名前があれば有効' do
      sample = described_class.new(name: 'テスト')
      expect(sample).to be_valid
    end
  end
end
