# frozen_string_literal: true

require 'rails_helper'

RSpec.describe AccountRepository, type: :repository do
  let(:repository) { described_class.new }

  describe '#find_all' do
    it 'すべての勘定科目を表示順で取得できる' do
      # Given: 複数の勘定科目を作成
      Account.create!(
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      )
      Account.create!(
        code: '2010',
        name: '買掛金',
        account_type: 'liability',
        bspl_type: 'B',
        debit_credit_type: 'C',
        transaction_type: '2',
        display_order: 200
      )

      # When: すべての勘定科目を取得
      accounts = repository.find_all

      # Then: 表示順で取得できる
      expect(accounts).to be_a(Array)
      expect(accounts.length).to eq(2)
      expect(accounts[0].code).to eq('1010')
      expect(accounts[1].code).to eq('2010')
    end

    it '勘定科目が存在しない場合は空配列を返す' do
      # When: 勘定科目が存在しない状態で取得
      accounts = repository.find_all

      # Then: 空配列を返す
      expect(accounts).to eq([])
    end
  end

  describe '#find_by_code' do
    it '科目コードで勘定科目を取得できる' do
      # Given: 勘定科目を作成
      Account.create!(
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      )

      # When: 科目コードで検索
      account = repository.find_by_code('1010')

      # Then: 勘定科目を取得できる
      expect(account).not_to be_nil
      expect(account.code).to eq('1010')
      expect(account.name).to eq('現金')
    end

    it '存在しない科目コードの場合はnilを返す' do
      # When: 存在しない科目コードで検索
      account = repository.find_by_code('9999')

      # Then: nilを返す
      expect(account).to be_nil
    end
  end

  describe '#find_by_bspl_type' do
    before do
      # Given: 貸借対照表科目と損益計算書科目を作成
      Account.create!(
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      )
      Account.create!(
        code: '4010',
        name: '売上高',
        account_type: 'revenue',
        bspl_type: 'P',
        debit_credit_type: 'C',
        transaction_type: '4',
        display_order: 400
      )
    end

    it 'BSPL区分がBの勘定科目を取得できる' do
      # When: BSPL区分='B'で検索
      accounts = repository.find_by_bspl_type('B')

      # Then: 貸借対照表科目のみ取得
      expect(accounts.length).to eq(1)
      expect(accounts[0].code).to eq('1010')
      expect(accounts[0].bspl_type).to eq('B')
    end

    it 'BSPL区分がPの勘定科目を取得できる' do
      # When: BSPL区分='P'で検索
      accounts = repository.find_by_bspl_type('P')

      # Then: 損益計算書科目のみ取得
      expect(accounts.length).to eq(1)
      expect(accounts[0].code).to eq('4010')
      expect(accounts[0].bspl_type).to eq('P')
    end
  end

  describe '#save' do
    it '新しい勘定科目を作成できる' do
      # Given: 新しい勘定科目
      account = Account.new(
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      )

      # When: 保存
      saved_account = repository.save(account)

      # Then: 保存される
      expect(saved_account).to be_persisted
      expect(saved_account.code).to eq('1010')
      expect(Account.find_by(code: '1010')).not_to be_nil
    end

    it '既存の勘定科目を更新できる' do
      # Given: 既存の勘定科目
      account = Account.create!(
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      )

      # When: 名前を更新
      account.name = '現金及び預金'
      saved_account = repository.save(account)

      # Then: 更新される
      expect(saved_account.name).to eq('現金及び預金')
      expect(Account.find_by(code: '1010').name).to eq('現金及び預金')
    end
  end

  describe '#delete_by_code' do
    it '科目コードで勘定科目を削除できる' do
      # Given: 勘定科目を作成
      Account.create!(
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      )

      # When: 削除
      repository.delete_by_code('1010')

      # Then: 削除される
      expect(Account.find_by(code: '1010')).to be_nil
    end

    it '存在しない科目コードの場合はエラーにならない' do
      # When & Then: 存在しない科目コードを削除してもエラーにならない
      expect { repository.delete_by_code('9999') }.not_to raise_error
    end
  end
end
