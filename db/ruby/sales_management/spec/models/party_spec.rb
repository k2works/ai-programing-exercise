# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Party, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:party_type) }
    it { is_expected.to validate_inclusion_of(:party_type).in_array(%w[Person Organization]) }
  end

  describe '関連' do
    it { is_expected.to have_one(:person).dependent(:destroy) }
    it { is_expected.to have_one(:organization).dependent(:destroy) }
    it { is_expected.to have_many(:party_roles).dependent(:destroy) }
  end

  describe '動作確認' do
    it '個人パーティを作成できる' do
      party = create(:party, :person)
      expect(party).to be_valid
      expect(party.person).to be_present
    end

    it '組織パーティを作成できる' do
      party = create(:party, :organization)
      expect(party).to be_valid
      expect(party.organization).to be_present
    end
  end
end
