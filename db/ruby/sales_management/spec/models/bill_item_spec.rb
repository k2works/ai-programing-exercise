# frozen_string_literal: true

require 'rails_helper'

RSpec.describe BillItem, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:amount) }
    it { is_expected.to validate_numericality_of(:amount).is_greater_than_or_equal_to(0) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:bill) }
    it { is_expected.to belong_to(:purchase) }
  end
end
