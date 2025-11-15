# frozen_string_literal: true

require 'rails_helper'

RSpec.describe NumberSequence, type: :model do
  describe 'バリデーション' do
    it { is_expected.to validate_presence_of(:sequence_type) }
    it { is_expected.to validate_presence_of(:prefix) }
  end

  describe 'ユニーク制約' do
    subject { build(:number_sequence) }

    it { is_expected.to validate_uniqueness_of(:sequence_type) }
  end

  describe '採番' do
    it '連番を生成できる' do
      sequence = create(:number_sequence, sequence_type: 'order', prefix: 'ORD', current_number: 0)

      number1 = sequence.next_number
      expect(number1).to match(/^ORD\d{8}\d{4}$/) # ORD + YYYYMMDD + 0001

      number2 = sequence.next_number
      expect(number2).to match(/^ORD\d{8}\d{4}$/)
      expect(number2).not_to eq(number1)
    end

    it '日付が変わると連番がリセットされる' do
      sequence = create(:number_sequence,
                        sequence_type: 'invoice',
                        prefix: 'INV',
                        current_number: 99,
                        last_generated_date: Date.yesterday)

      number = sequence.next_number
      # 新しい日付なので0001から始まる
      expect(number).to end_with('0001')
    end

    it '同時アクセスでも一意な番号を生成する' do
      skip 'This test requires database_cleaner strategy :truncation for multi-threading'
      sequence = create(:number_sequence, sequence_type: 'order', prefix: 'ORD')

      # 複数スレッドで同時に採番
      numbers = 10.times.map do
        Thread.new { sequence.next_number }
      end.map(&:value)

      # すべて一意であることを確認
      expect(numbers.uniq.length).to eq(10)
    end
  end
end
