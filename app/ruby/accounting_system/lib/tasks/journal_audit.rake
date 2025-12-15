# frozen_string_literal: true

namespace :journal do
  desc '仕訳の複式簿記チェックを実行'
  task audit: :environment do
    puts '仕訳の複式簿記チェックを開始します...'

    if JournalBalanceChecker.audit_all_journals
      puts '✓ すべての仕訳が貸借平衡しています'
    else
      puts '✗ 貸借が一致しない仕訳が見つかりました（詳細はログを確認してください）'
      exit 1
    end
  end
end
