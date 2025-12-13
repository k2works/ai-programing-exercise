# frozen_string_literal: true

require 'rails_helper'

RSpec.describe Account, type: :model do
  describe 'バリデーション' do
    it '勘定科目コードと勘定科目名があれば有効' do
      account = described_class.new(
        code: '1000',
        name: '現金',
        account_type: :asset,
        balance: 50_000.00
      )
      expect(account).to be_valid
    end

    it '勘定科目コードがなければ無効' do
      account = described_class.new(
        name: '現金',
        account_type: :asset
      )
      expect(account).not_to be_valid
      expect(account.errors[:code]).to include("can't be blank")
    end

    it '勘定科目名がなければ無効' do
      account = described_class.new(
        code: '1000',
        account_type: :asset
      )
      expect(account).not_to be_valid
      expect(account.errors[:name]).to include("can't be blank")
    end

    it '勘定科目コードは一意でなければならない' do
      described_class.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        balance: 50_000.00
      )

      duplicate = described_class.new(
        code: '1000',
        name: '普通預金',
        account_type: :asset
      )
      expect(duplicate).not_to be_valid
      expect(duplicate.errors[:code]).to include('has already been taken')
    end
  end

  describe 'CRUD 操作' do
    it '勘定科目を作成できる' do
      account = described_class.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        balance: 50_000.00
      )

      expect(account).to be_persisted
      expect(account.code).to eq('1000')
      expect(account.name).to eq('現金')
      expect(account.account_type).to eq('asset')
      expect(account.balance).to eq(50_000.00)
    end

    it 'すべての勘定科目を取得できる' do
      described_class.create!(code: '1000', name: '現金', account_type: :asset, balance: 50_000.00)
      described_class.create!(code: '2000', name: '買掛金', account_type: :liability, balance: 30_000.00)
      described_class.create!(code: '3000', name: '資本金', account_type: :equity, balance: 100_000.00)

      accounts = described_class.order(:code)
      expect(accounts.count).to eq(3)
      expect(accounts.pluck(:code)).to eq(%w[1000 2000 3000])
    end

    it '勘定科目コードで検索できる' do
      described_class.create!(code: '1000', name: '現金', account_type: :asset, balance: 50_000.00)

      account = described_class.find_by(code: '1000')
      expect(account).not_to be_nil
      expect(account.name).to eq('現金')
      expect(account.account_type).to eq('asset')
    end

    it '勘定科目を更新できる' do
      account = described_class.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        balance: 50_000.00
      )

      account.update!(name: '現金及び預金', balance: 75_000.00)

      updated = described_class.find(account.id)
      expect(updated.name).to eq('現金及び預金')
      expect(updated.balance).to eq(75_000.00)
      expect(updated.code).to eq('1000') # 変更していない項目は保持される
    end

    it '勘定科目を削除できる' do
      account = described_class.create!(
        code: '1000',
        name: '現金',
        account_type: :asset,
        balance: 50_000.00
      )

      account.destroy!

      expect(described_class.find_by(id: account.id)).to be_nil
      expect(described_class.count).to eq(0)
    end
  end

  describe '勘定科目種別でのフィルタリング' do
    before do
      described_class.create!(code: '1000', name: '現金', account_type: :asset, balance: 50_000.00)
      described_class.create!(code: '2000', name: '買掛金', account_type: :liability, balance: 30_000.00)
      described_class.create!(code: '3000', name: '資本金', account_type: :equity, balance: 100_000.00)
    end

    it '資産勘定のみを取得できる' do
      assets = described_class.where(account_type: :asset)
      expect(assets.count).to eq(1)
      expect(assets.first.name).to eq('現金')
    end

    it '負債勘定のみを取得できる' do
      liabilities = described_class.where(account_type: :liability)
      expect(liabilities.count).to eq(1)
      expect(liabilities.first.name).to eq('買掛金')
    end
  end
end
