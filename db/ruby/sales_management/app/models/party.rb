# frozen_string_literal: true

class Party < ApplicationRecord
  TYPES = %w[Person Organization].freeze

  validates :party_type, presence: true, inclusion: { in: TYPES }

  has_one :person, dependent: :destroy
  has_one :organization, dependent: :destroy
  has_many :party_roles, dependent: :destroy
  has_one :credit_limit, dependent: :destroy
  has_many :invoices, dependent: :restrict_with_error

  # 役割チェックメソッド
  def customer?
    party_roles.customers.active.exists?
  end

  def supplier?
    party_roles.suppliers.active.exists?
  end
end
