# frozen_string_literal: true

class PurchaseOrder < ApplicationRecord
  STATUSES = %w[draft submitted received cancelled].freeze

  belongs_to :party
  belongs_to :warehouse
  belongs_to :sales_order, class_name: 'Order', optional: true
  has_many :purchase_order_items, dependent: :destroy
  has_many :products, through: :purchase_order_items
  has_many :purchases, dependent: :restrict_with_error

  validates :order_number, presence: true, uniqueness: true
  validates :order_date, presence: true
  validates :status, inclusion: { in: STATUSES }

  before_validation :set_defaults, on: :create

  # スコープ
  scope :by_status, ->(status) { where(status: status) }
  scope :pending, -> { where(status: %w[draft submitted]) }

  # ステータスチェック
  STATUSES.each do |status_name|
    define_method("#{status_name}?") do
      status == status_name
    end
  end

  # ステータス変更
  def submit!
    update!(status: 'submitted')
  end

  def receive!
    raise '全明細が入荷完了していません' unless all_received?

    update!(status: 'received')
  end

  def cancel!
    update!(status: 'cancelled')
  end

  # 入荷完了チェック
  def all_received?
    purchase_order_items.all? { |item| item.quantity == item.received_quantity }
  end

  # 合計金額計算
  def calculate_total
    purchase_order_items.sum(&:subtotal)
  end

  private

  def set_defaults
    self.status ||= 'draft'
    self.order_date ||= Date.current
  end
end
