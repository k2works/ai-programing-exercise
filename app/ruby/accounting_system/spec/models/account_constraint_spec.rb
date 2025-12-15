# frozen_string_literal: true

require 'rails_helper'

RSpec.describe 'Account Constraints', type: :model do
  describe 'BSPL区分の制約' do
    it "BSPL区分は 'B' または 'P' のみ許可" do
      account = build(:account, bspl_type: 'X')

      expect(account).not_to be_valid
      expect(account.errors[:bspl_type]).to include('は B または P のみ許可されています')
    end

    it "資産科目のBSPL区分は 'B' である必要がある" do
      account = build(:account, :asset, bspl_type: 'P')

      expect(account).not_to be_valid
      expect(account.errors[:bspl_type]).to include('資産・負債・純資産は B (貸借対照表) を設定してください')
    end

    it "収益科目のBSPL区分は 'P' である必要がある" do
      account = build(:account, :revenue, bspl_type: 'B')

      expect(account).not_to be_valid
      expect(account.errors[:bspl_type]).to include('収益・費用は P (損益計算書) を設定してください')
    end
  end

  describe '取引要素区分の制約' do
    it "取引要素区分は '1'〜'5' のみ許可" do
      account = build(:account, transaction_type: '9')

      expect(account).not_to be_valid
      expect(account.errors[:transaction_type]).to include('は 1〜5 のみ許可されています')
    end

    it '資産科目の取引要素区分は 1 である必要がある' do
      account = build(:account, :asset, bspl_type: 'B', transaction_type: '2')

      expect(account).not_to be_valid
      expect(account.errors[:transaction_type]).to include('資産の取引要素区分は 1 である必要があります')
    end

    it '負債科目の取引要素区分は 2 である必要がある' do
      account = build(:account, :liability, bspl_type: 'B', transaction_type: '1')

      expect(account).not_to be_valid
      expect(account.errors[:transaction_type]).to include('負債の取引要素区分は 2 である必要があります')
    end

    it '費用科目の取引要素区分は 5 である必要がある' do
      account = build(:account, :expense, bspl_type: 'P', transaction_type: '4')

      expect(account).not_to be_valid
      expect(account.errors[:transaction_type]).to include('費用の取引要素区分は 5 である必要があります')
    end
  end

  describe '費用区分の制約' do
    it '費用区分は費用科目のみ設定可能' do
      account = build(:account, :asset, bspl_type: 'B', transaction_type: '1', expense_type: '1')

      expect(account).not_to be_valid
      expect(account.errors[:expense_type]).to include('は費用科目のみ設定可能です')
    end

    it '費用科目の費用区分設定は有効' do
      account = build(:account, :expense, bspl_type: 'P', transaction_type: '5', expense_type: '1')

      expect(account).to be_valid
    end

    it "費用区分は '1'〜'3' のみ許可" do
      account = build(:account, :expense, bspl_type: 'P', transaction_type: '5', expense_type: '9')

      expect(account).not_to be_valid
      expect(account.errors[:expense_type]).to include('は 1〜3 のみ許可されています')
    end
  end
end
