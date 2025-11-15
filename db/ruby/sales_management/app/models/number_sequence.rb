# frozen_string_literal: true

class NumberSequence < ApplicationRecord
  validates :sequence_type, presence: true, uniqueness: true
  validates :prefix, presence: true

  before_validation :set_defaults, on: :create

  # 次の番号を生成（トランザクションでロック）
  def next_number
    self.class.transaction do
      # 悲観的ロックで同時アクセスを制御
      reload
      lock!

      # 日付が変わっていればリセット
      if last_generated_date != Date.current
        self.current_number = 0
        self.last_generated_date = Date.current
      end

      # インクリメント
      self.current_number += 1
      next_num = current_number
      save!

      # フォーマット: PREFIX + YYYYMMDD + 0001
      format_number(next_num)
    end
  end

  private

  def set_defaults
    self.current_number ||= 0
    self.last_generated_date ||= Date.current
  end

  def format_number(number)
    date_part = Date.current.strftime('%Y%m%d')
    seq_part = number.to_s.rjust(4, '0')
    "#{prefix}#{date_part}#{seq_part}"
  end
end
