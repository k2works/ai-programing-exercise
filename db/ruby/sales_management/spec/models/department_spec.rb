# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Department, type: :model do
  describe 'バリデーション' do
    it '部門コードと部門名があれば有効' do
      department = described_class.new(
        code: 'D001',
        name: '営業部'
      )
      expect(department).to be_valid
    end

    it '部門コードがなければ無効' do
      department = described_class.new(name: '営業部')
      expect(department).not_to be_valid
      expect(department.errors[:code]).to include("can't be blank")
    end

    it '部門名がなければ無効' do
      department = described_class.new(code: 'D001')
      expect(department).not_to be_valid
      expect(department.errors[:name]).to include("can't be blank")
    end

    it '部門コードは一意でなければならない' do
      described_class.create!(code: 'D001', name: '営業部')
      duplicate = described_class.new(code: 'D001', name: '総務部')
      expect(duplicate).not_to be_valid
      expect(duplicate.errors[:code]).to include('has already been taken')
    end
  end

  describe '関連' do
    it '複数の社員を持つことができる' do
      department = described_class.new(code: 'D001', name: '営業部')
      expect(department).to respond_to(:employees)
    end
  end
end
