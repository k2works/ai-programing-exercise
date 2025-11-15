# frozen_string_literal: true

class Order < ApplicationRecord
  TYPES = %w[Sales Purchase].freeze
  STATUSES = %w[draft confirmed completed cancelled].freeze

  belongs_to :party
  has_many :order_items, dependent: :destroy
  has_many :products, through: :order_items

  validates :order_number, presence: true, uniqueness: true
  validates :order_type, presence: true, inclusion: { in: TYPES }
  validates :order_date, presence: true
  validates :status, inclusion: { in: STATUSES }

  # デフォルト値
  before_validation :set_defaults, on: :create
  before_validation :generate_order_number, on: :create
  before_create :check_credit_limit

  # スコープ
  scope :sales, -> { where(order_type: 'Sales') }
  scope :purchases, -> { where(order_type: 'Purchase') }
  scope :by_status, ->(status) { where(status: status) }

  # ステータスチェックメソッド
  STATUSES.each do |status_name|
    define_method("#{status_name}?") do
      status == status_name
    end
  end

  # ステータス変更メソッド
  def confirm!
    update!(status: 'confirmed')
  end

  def complete!
    update!(status: 'completed')
  end

  def cancel!
    update!(status: 'cancelled')
  end

  # 合計金額計算
  def total_amount
    order_items.sum(&:subtotal)
  end

  def calculate_total
    order_items.sum { |item| item.quantity * item.unit_price }
  end

  private

  def set_defaults
    self.status ||= 'draft'
    self.order_date ||= Date.current
  end

  def generate_order_number
    return if order_number.present?

    sequence_type = order_type == 'Sales' ? 'sales_order' : 'purchase_order'
    sequence = NumberSequence.find_or_create_by!(
      sequence_type: sequence_type,
      prefix: order_type == 'Sales' ? 'SO' : 'PO'
    )
    self.order_number = sequence.next_number
  end

  def check_credit_limit
    return unless order_type == 'Sales'

    credit_limit = party.credit_limit
    return unless credit_limit

    total = calculate_total
    return if credit_limit.available_with_outstanding?(total)

    errors.add(:base, '与信限度額を超えています')
    throw :abort
  end
end
