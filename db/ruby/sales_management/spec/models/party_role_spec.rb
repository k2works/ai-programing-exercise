# frozen_string_literal: true

require 'rails_helper'

RSpec.describe PartyRole, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:role_type) }
    it { is_expected.to validate_inclusion_of(:role_type).in_array(%w[Customer Supplier]) }
  end

  describe '関連' do
    it { is_expected.to belong_to(:party) }
  end

  describe 'スコープ' do
    let(:party) { create(:party, :organization) }
    let!(:customer_role) { create(:party_role, :customer, party: party) }
    let!(:supplier_role) { create(:party_role, :supplier, party: party) }
    let!(:expired_role) { create(:party_role, :customer, party: party, ended_at: 1.day.ago) }

    it '顧客役割のみ取得できる' do
      expect(described_class.customers).to include(customer_role)
      expect(described_class.customers).not_to include(supplier_role)
    end

    it '有効な役割のみ取得できる' do
      expect(described_class.active).to include(customer_role, supplier_role)
      expect(described_class.active).not_to include(expired_role)
    end
  end
end
