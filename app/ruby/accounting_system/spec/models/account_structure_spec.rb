# frozen_string_literal: true

require 'rails_helper'

RSpec.describe AccountStructure, type: :model do
  describe 'バリデーション' do
    let(:account) { create(:account, code: '11000') }

    it '有効なレコードを作成できる' do
      structure = AccountStructure.create!(
        account_code: account.code,
        account_path: '11000',
        hierarchy_level: 1,
        display_order: 0
      )

      expect(structure).to be_persisted
    end

    it 'account_code は必須' do
      structure = build(:account_structure, account_code: nil)

      expect(structure).not_to be_valid
      expect(structure.errors[:account_code]).to be_present
    end

    it 'account_path は必須' do
      structure = build(:account_structure, account_path: nil)

      expect(structure).not_to be_valid
      expect(structure.errors[:account_path]).to be_present
    end

    it 'hierarchy_level は1以上の整数' do
      structure = build(:account_structure, hierarchy_level: 0)

      expect(structure).not_to be_valid
      expect(structure.errors[:hierarchy_level]).to be_present
    end
  end

  describe '階層構造' do
    let!(:root) do
      account = create(:account, code: '11', name: '資産の部')
      create(:account_structure,
             account_code: account.code,
             account_path: '11',
             hierarchy_level: 1,
             parent_code: nil,
             display_order: 0)
    end

    let!(:level2) do
      account = create(:account, code: '11000', name: '流動資産')
      create(:account_structure,
             account_code: account.code,
             account_path: '11~11000',
             hierarchy_level: 2,
             parent_code: '11',
             display_order: 0)
    end

    let!(:level3) do
      account = create(:account, code: '11190', name: '現金及び預金')
      create(:account_structure,
             account_code: account.code,
             account_path: '11~11000~11190',
             hierarchy_level: 3,
             parent_code: '11000',
             display_order: 0)
    end

    let!(:level4_cash) do
      account = create(:account, code: '11110', name: '現金')
      create(:account_structure,
             account_code: account.code,
             account_path: '11~11000~11190~11110',
             hierarchy_level: 4,
             parent_code: '11190',
             display_order: 0)
    end

    let!(:level4_deposit) do
      account = create(:account, code: '11120', name: '当座預金')
      create(:account_structure,
             account_code: account.code,
             account_path: '11~11000~11190~11120',
             hierarchy_level: 4,
             parent_code: '11190',
             display_order: 1)
    end

    describe '.find_children' do
      it '特定科目配下のすべての子孫を取得できる' do
        children = AccountStructure.find_children('11190')

        expect(children.pluck(:account_code)).to contain_exactly(
          '11190', '11110', '11120'
        )
      end

      it 'ルート科目配下のすべての子孫を取得できる' do
        children = AccountStructure.find_children('11')

        expect(children.pluck(:account_code)).to contain_exactly(
          '11', '11000', '11190', '11110', '11120'
        )
      end
    end

    describe '#path_codes' do
      it '階層パスを配列として取得できる' do
        expect(level4_cash.path_codes).to eq(%w[11 11000 11190 11110])
      end
    end

    describe '#parent_structure' do
      it '親のAccountStructureを取得できる' do
        parent = level3.parent_structure

        expect(parent).to eq(level2)
      end

      it 'ルート要素の親はnil' do
        expect(root.parent_structure).to be_nil
      end
    end

    describe '#children' do
      it '子のAccountStructureを取得できる' do
        children = level3.children

        expect(children.pluck(:account_code)).to contain_exactly('11110', '11120')
      end
    end
  end
end
