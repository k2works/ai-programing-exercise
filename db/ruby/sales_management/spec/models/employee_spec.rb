# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Employee, type: :model do
  let(:department) { Department.create!(code: 'D001', name: '営業部') }

  describe 'バリデーション' do
    it '社員コード、姓、名、部門があれば有効' do
      employee = described_class.new(
        code: 'E001',
        first_name: '太郎',
        last_name: '山田',
        department:
      )
      expect(employee).to be_valid
    end

    it '社員コードがなければ無効' do
      employee = described_class.new(
        first_name: '太郎',
        last_name: '山田',
        department:
      )
      expect(employee).not_to be_valid
      expect(employee.errors[:code]).to include("can't be blank")
    end

    it '社員コードは一意でなければならない' do
      described_class.create!(
        code: 'E001',
        first_name: '太郎',
        last_name: '山田',
        department:
      )
      duplicate = described_class.new(
        code: 'E001',
        first_name: '花子',
        last_name: '佐藤',
        department:
      )
      expect(duplicate).not_to be_valid
      expect(duplicate.errors[:code]).to include('has already been taken')
    end

    it '部門がなければ無効' do
      employee = described_class.new(
        code: 'E001',
        first_name: '太郎',
        last_name: '山田'
      )
      expect(employee).not_to be_valid
      expect(employee.errors[:department]).to include('must exist')
    end
  end

  describe '関連' do
    it '部門に属する' do
      employee = described_class.new(
        code: 'E001',
        first_name: '太郎',
        last_name: '山田',
        department:
      )
      expect(employee.department).to eq(department)
    end
  end

  describe 'メソッド' do
    it 'フルネームを返す' do
      employee = described_class.new(
        code: 'E001',
        first_name: '太郎',
        last_name: '山田',
        department:
      )
      expect(employee.full_name).to eq('山田 太郎')
    end
  end
end
