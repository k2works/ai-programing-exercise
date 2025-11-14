# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Organization, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:name) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
  end
end
