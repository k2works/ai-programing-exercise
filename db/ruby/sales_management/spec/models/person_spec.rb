# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Person, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:first_name) }
    it { is_expected.to validate_presence_of(:last_name) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
  end

  describe 'メソッド' do
    it 'フルネームを返す' do
      person = build(:person, first_name: '太郎', last_name: '山田')
      expect(person.full_name).to eq('山田 太郎')
    end
  end
end
