# frozen_string_literal: true

# 勘定科目構成モデル
# 勘定科目の階層構造をチルダ連結方式で管理する
class AccountStructure < ApplicationRecord
  self.primary_key = 'account_code'

  # アソシエーション
  belongs_to :account, foreign_key: :account_code, primary_key: :code

  # バリデーション
  validates :account_code, presence: true, uniqueness: true
  validates :account_path, presence: true
  validates :hierarchy_level, presence: true, numericality: {
    only_integer: true,
    greater_than: 0
  }
  validates :display_order, presence: true, numericality: { only_integer: true }

  # スコープ
  scope :by_level, ->(level) { where(hierarchy_level: level).order(:display_order, :account_code) }
  scope :ordered, -> { order(:account_path) }

  # 特定科目配下の子孫を取得（自身を含む）
  def self.find_children(account_code)
    where('account_path LIKE ? OR account_path LIKE ? OR account_code = ?',
          "#{account_code}~%", "%~#{account_code}~%", account_code)
      .ordered
  end

  # 階層パスから配列を取得
  def path_codes
    account_path.split('~')
  end

  # 親の AccountStructure を取得
  def parent_structure
    return nil unless parent_code

    self.class.find_by(account_code: parent_code)
  end

  # 子の AccountStructure を取得
  def children
    self.class.where(parent_code: account_code)
  end
end
