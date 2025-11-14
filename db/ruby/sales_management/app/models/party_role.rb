# frozen_string_literal: true

class PartyRole < ApplicationRecord
  ROLES = %w[Customer Supplier].freeze

  belongs_to :party

  validates :role_type, presence: true, inclusion: { in: ROLES }

  # スコープ
  scope :customers, -> { where(role_type: 'Customer') }
  scope :suppliers, -> { where(role_type: 'Supplier') }
  scope :active, -> { where('ended_at IS NULL OR ended_at > ?', Time.current) }
end
