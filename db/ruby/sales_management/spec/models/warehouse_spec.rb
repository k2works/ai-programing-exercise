# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Warehouse, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:code) }
    it { is_expected.to validate_presence_of(:name) }
    it { is_expected.to validate_uniqueness_of(:code) }
    it { is_expected.to validate_inclusion_of(:warehouse_type).in_array([1, 2]) }
  end

  describe '関連' do
    it { is_expected.to have_many(:stocks).dependent(:destroy) }
    it { is_expected.to have_many(:purchase_orders).dependent(:restrict_with_error) }
  end
end
