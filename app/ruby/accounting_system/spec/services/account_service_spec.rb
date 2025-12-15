# frozen_string_literal: true

require 'rails_helper'

RSpec.describe AccountService, type: :service do
  let(:repository) { instance_double(AccountRepository) }
  let(:service) { described_class.new(account_repository: repository) }

  describe '#all_accounts' do
    it 'すべての勘定科目を取得できる' do
      # Given: リポジトリから勘定科目リストを返す
      accounts = [
        Account.new(code: '1010', name: '現金'),
        Account.new(code: '2010', name: '買掛金')
      ]
      allow(repository).to receive(:find_all).and_return(accounts)

      # When: すべての勘定科目を取得
      result = service.all_accounts

      # Then: リポジトリから取得した勘定科目を返す
      expect(result).to eq(accounts)
      expect(repository).to have_received(:find_all)
    end
  end

  describe '#find_account' do
    it '科目コードで勘定科目を取得できる' do
      # Given: リポジトリから勘定科目を返す
      account = Account.new(code: '1010', name: '現金')
      allow(repository).to receive(:find_by_code).with('1010').and_return(account)

      # When: 科目コードで検索
      result = service.find_account('1010')

      # Then: 勘定科目を返す
      expect(result).to eq(account)
      expect(repository).to have_received(:find_by_code).with('1010')
    end

    it '勘定科目が見つからない場合はエラーを発生させる' do
      # Given: リポジトリからnilを返す
      allow(repository).to receive(:find_by_code).with('9999').and_return(nil)

      # When & Then: エラーを発生させる
      expect { service.find_account('9999') }.to raise_error(AccountNotFoundError, '科目コード 9999 が見つかりません')
    end
  end

  describe '#find_accounts_by_bspl_type' do
    it 'BSPL区分がBの勘定科目を取得できる' do
      # Given: リポジトリから貸借対照表科目を返す
      accounts = [Account.new(code: '1010', name: '現金', bspl_type: 'B')]
      allow(repository).to receive(:find_by_bspl_type).with('B').and_return(accounts)

      # When: BSPL区分='B'で検索
      result = service.find_accounts_by_bspl_type('B')

      # Then: 貸借対照表科目を返す
      expect(result).to eq(accounts)
      expect(repository).to have_received(:find_by_bspl_type).with('B')
    end

    it 'BSPL区分がPの勘定科目を取得できる' do
      # Given: リポジトリから損益計算書科目を返す
      accounts = [Account.new(code: '4010', name: '売上高', bspl_type: 'P')]
      allow(repository).to receive(:find_by_bspl_type).with('P').and_return(accounts)

      # When: BSPL区分='P'で検索
      result = service.find_accounts_by_bspl_type('P')

      # Then: 損益計算書科目を返す
      expect(result).to eq(accounts)
      expect(repository).to have_received(:find_by_bspl_type).with('P')
    end

    it '不正なBSPL区分の場合はエラーを発生させる' do
      # When & Then: 不正な値でエラーを発生させる
      expect { service.find_accounts_by_bspl_type('X') }.to raise_error(ArgumentError, "BSPL区分は 'B' または 'P' である必要があります")
    end
  end

  describe '#create_account' do
    it '新しい勘定科目を作成できる' do
      # Given: 新しい勘定科目の属性
      attributes = {
        code: '1010',
        name: '現金',
        account_type: 'asset',
        bspl_type: 'B',
        debit_credit_type: 'D',
        transaction_type: '1',
        display_order: 100
      }

      # リポジトリのモック設定
      allow(repository).to receive(:find_by_code).with('1010').and_return(nil)
      allow(repository).to receive(:save) do |account|
        account.id = 1
        account
      end

      # When: 勘定科目を作成
      result = service.create_account(attributes)

      # Then: 勘定科目が作成される
      expect(result.code).to eq('1010')
      expect(result.name).to eq('現金')
      expect(repository).to have_received(:find_by_code).with('1010')
      expect(repository).to have_received(:save)
    end

    it '重複する科目コードの場合はエラーを発生させる' do
      # Given: 既存の勘定科目
      existing = Account.new(code: '1010', name: '現金')
      allow(repository).to receive(:find_by_code).with('1010').and_return(existing)

      # When & Then: 重複エラーを発生させる
      attributes = { code: '1010', name: '新しい現金' }
      expect { service.create_account(attributes) }.to raise_error(DuplicateAccountError, '科目コード 1010 は既に存在します')
    end

    it '勘定科目コードが空の場合はエラーを発生させる' do
      # Given: 科目コードが空の属性
      attributes = { code: '', name: '現金' }
      allow(repository).to receive(:find_by_code).with('').and_return(nil)

      # When & Then: バリデーションエラーを発生させる
      expect { service.create_account(attributes) }.to raise_error(ArgumentError, '勘定科目コードは必須です')
    end

    it '勘定科目名が空の場合はエラーを発生させる' do
      # Given: 科目名が空の属性
      attributes = { code: '1010', name: '' }
      allow(repository).to receive(:find_by_code).with('1010').and_return(nil)

      # When & Then: バリデーションエラーを発生させる
      expect { service.create_account(attributes) }.to raise_error(ArgumentError, '勘定科目名は必須です')
    end

    it 'BSPL区分が不正な場合はエラーを発生させる' do
      # Given: 不正なBSPL区分
      attributes = { code: '1010', name: '現金', bspl_type: 'X' }
      allow(repository).to receive(:find_by_code).with('1010').and_return(nil)

      # When & Then: バリデーションエラーを発生させる
      expect { service.create_account(attributes) }.to raise_error(ArgumentError, "BSPL区分は 'B' または 'P' である必要があります")
    end
  end

  describe '#update_account' do
    it '既存の勘定科目を更新できる' do
      # Given: 既存の勘定科目
      account = Account.new(code: '1010', name: '現金', bspl_type: 'B')
      allow(repository).to receive(:find_by_code).with('1010').and_return(account)
      allow(repository).to receive(:save) do |acc|
        acc
      end

      # When: 勘定科目を更新
      result = service.update_account('1010', { name: '現金及び預金' })

      # Then: 更新される
      expect(result.name).to eq('現金及び預金')
      expect(repository).to have_received(:save)
    end

    it '存在しない勘定科目を更新しようとするとエラーを発生させる' do
      # Given: 存在しない勘定科目
      allow(repository).to receive(:find_by_code).with('9999').and_return(nil)

      # When & Then: エラーを発生させる
      expect { service.update_account('9999', { name: '新しい名前' }) }.to raise_error(AccountNotFoundError)
    end
  end

  describe '#delete_account' do
    it '勘定科目を削除できる' do
      # Given: 既存の勘定科目
      account = Account.new(code: '1010', name: '現金')
      allow(repository).to receive(:find_by_code).with('1010').and_return(account)
      allow(repository).to receive(:delete_by_code).with('1010')

      # When: 勘定科目を削除
      service.delete_account('1010')

      # Then: 削除される
      expect(repository).to have_received(:delete_by_code).with('1010')
    end

    it '存在しない勘定科目を削除しようとするとエラーを発生させる' do
      # Given: 存在しない勘定科目
      allow(repository).to receive(:find_by_code).with('9999').and_return(nil)

      # When & Then: エラーを発生させる
      expect { service.delete_account('9999') }.to raise_error(AccountNotFoundError)
    end
  end
end
