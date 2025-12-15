# frozen_string_literal: true

# アプリケーション層：Application Service
# ビジネスロジックを実装
class AccountService
  # @param account_repository [AccountRepositoryInterface] リポジトリ
  def initialize(account_repository: AccountRepository.new)
    @account_repository = account_repository
  end

  # すべての勘定科目を取得
  # @return [Array<Account>]
  def all_accounts
    @account_repository.find_all
  end

  # 科目コードで勘定科目を取得
  # @param account_code [String] 科目コード
  # @return [Account]
  # @raise [AccountNotFoundError] 勘定科目が見つからない場合
  def find_account(account_code)
    account = @account_repository.find_by_code(account_code)
    raise AccountNotFoundError, "科目コード #{account_code} が見つかりません" if account.nil?

    account
  end

  # BSPL 区分で勘定科目を取得
  # @param bspl_type [String] BSPL 区分（'B' または 'P'）
  # @return [Array<Account>]
  # @raise [ArgumentError] BSPL 区分が不正な場合
  def find_accounts_by_bspl_type(bspl_type)
    unless %w[B P].include?(bspl_type)
      raise ArgumentError, "BSPL区分は 'B' または 'P' である必要があります"
    end

    @account_repository.find_by_bspl_type(bspl_type)
  end

  # 新しい勘定科目を作成
  # @param attributes [Hash] 勘定科目の属性
  # @return [Account]
  # @raise [DuplicateAccountError] 科目コードが重複している場合
  def create_account(attributes)
    account = Account.new(attributes)

    # 重複チェック
    existing = @account_repository.find_by_code(account.code)
    if existing
      raise DuplicateAccountError, "科目コード #{account.code} は既に存在します"
    end

    # ビジネスルール検証
    validate_account(account)

    # トランザクション内で保存
    ActiveRecord::Base.transaction do
      @account_repository.save(account)
    end

    account
  end

  # 勘定科目を更新
  # @param account_code [String] 科目コード
  # @param attributes [Hash] 更新する属性
  # @return [Account]
  # @raise [AccountNotFoundError] 勘定科目が見つからない場合
  def update_account(account_code, attributes)
    account = find_account(account_code)
    account.assign_attributes(attributes)

    # ビジネスルール検証
    validate_account(account)

    # トランザクション内で保存
    ActiveRecord::Base.transaction do
      @account_repository.save(account)
    end

    account
  end

  # 勘定科目を削除
  # @param account_code [String] 科目コード
  # @return [void]
  # @raise [AccountNotFoundError] 勘定科目が見つからない場合
  def delete_account(account_code)
    # 存在チェック
    find_account(account_code)

    # トランザクション内で削除
    ActiveRecord::Base.transaction do
      @account_repository.delete_by_code(account_code)
    end
  end

  private

  # ビジネスルール検証
  # @param account [Account] 勘定科目
  # @raise [ArgumentError] 検証エラー
  def validate_account(account)
    # 勘定科目コードは必須
    if account.code.blank?
      raise ArgumentError, '勘定科目コードは必須です'
    end

    # 勘定科目名は必須
    if account.name.blank?
      raise ArgumentError, '勘定科目名は必須です'
    end

    # BSPL 区分は 'B' または 'P'
    unless %w[B P].include?(account.bspl_type)
      raise ArgumentError, "BSPL区分は 'B' または 'P' である必要があります"
    end
  end
end

# カスタム例外
class AccountNotFoundError < StandardError; end
class DuplicateAccountError < StandardError; end
