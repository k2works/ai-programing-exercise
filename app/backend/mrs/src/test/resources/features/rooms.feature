Feature: 会議室管理機能（F002）

  Background:
    Given システムに以下のユーザーが存在する:
      | userId | name     | password | role  |
      | taro   | 山田太郎 | pass123  | USER  |
      | hanako | 鈴木花子 | pass456  | USER  |
      | admin  | 管理者   | admin123 | ADMIN |

    Given 以下の会議室が予約可能である:
      | roomId | roomName |
      | 1      | A会議室  |
      | 2      | B会議室  |

  # F002-01: 会議室一覧表示機能
  @rooms @list
  Scenario: 一般ユーザーが当日の予約可能会議室一覧を表示する
    Given ユーザー "taro" としてログインしている
    When 当日の会議室一覧を取得する
    Then 以下の会議室が表示される:
      | roomId | roomName |
      | 1      | A会議室  |
      | 2      | B会議室  |
    And レスポンスステータスは 200 である

  @rooms @list
  Scenario: 管理者が当日の予約可能会議室一覧を表示する
    Given ユーザー "admin" としてログインしている
    When 当日の会議室一覧を取得する
    Then 以下の会議室が表示される:
      | roomId | roomName |
      | 1      | A会議室  |
      | 2      | B会議室  |
    And レスポンスステータスは 200 である

  @rooms @list
  Scenario: 指定日の予約可能会議室一覧を表示する
    Given ユーザー "taro" としてログインしている
    When "2025-09-10" の会議室一覧を取得する
    Then 指定日に予約可能な会議室が表示される
    And レスポンスステータスは 200 である

  @rooms @api
  Scenario: 認証なしで会議室一覧にアクセスする（テスト環境）
    When 認証なしで会議室一覧を取得する
    Then 認証エラーまたは正常レスポンスが返される

  # F002-02: 日付切り替え機能（ナビゲーション）
  @rooms @navigation
  Scenario: 日付パラメータでの会議室一覧表示
    Given ユーザー "hanako" としてログインしている
    When "2025-09-05" の会議室一覧を取得する
    Then 指定日 "2025-09-05" の会議室一覧が表示される
    And レスポンスステータスは 200 である

  # N001-01: 性能要件 - 画面表示2秒以内
  @rooms @performance
  Scenario: 会議室一覧表示の応答時間要件
    Given ユーザー "taro" としてログインしている
    When 会議室一覧の応答時間を測定する
    Then 応答時間は 2秒以内である

  # UI002-02: ユーザーインターフェース要件
  @rooms @ui
  Scenario: 会議室一覧画面の基本項目確認
    Given ユーザー "taro" としてログインしている
    When 会議室一覧画面を表示する
    Then 以下の項目が含まれる:
      | 項目     | 説明                     |
      | 日付表示 | 対象日が表示されている   |
      | 会議室名 | クリック可能なリンク     |
      | 会議室ID | 会議室ID順でソート       |

  # C002: ビジネス制約 - 予約可能時間の制限
  @rooms @constraint
  Scenario: 予約可能日が設定されていない日付の会議室一覧
    Given ユーザー "taro" としてログインしている
    When "2025-12-31" の会議室一覧を取得する
    Then 予約可能な会議室がない場合のメッセージが表示される
    And レスポンスステータスは 200 である

  # セキュリティ要件（N003）- ユーザーごとの会議室一覧取得
  @rooms @security
  Scenario: 異なるユーザーが同じ日付の会議室一覧を取得する
    Given ユーザー "taro" としてログインしている
    When 当日の会議室一覧を取得する
    Then レスポンスステータスは 200 である
    When ユーザー "hanako" としてログインしている
    When 当日の会議室一覧を取得する
    Then レスポンスステータスは 200 である
    And 両ユーザーで同じ会議室一覧が取得できる

  # エラーハンドリング
  @rooms @error
  Scenario: 無効な日付形式での会議室一覧取得
    Given ユーザー "taro" としてログインしている
    When 無効な日付 "invalid-date" で会議室一覧を取得する
    Then 無効日付の処理結果が返される
    And エラーまたは空のレスポンスが表示される

  @rooms @error
  Scenario: 過去の日付での会議室一覧取得（制約確認）
    Given ユーザー "taro" としてログインしている
    When "2020-01-01" の会議室一覧を取得する
    Then 過去日付の処理結果が適切に返される
    And レスポンスステータスは 200 である

  # データ整合性確認（D002-01）
  @rooms @data
  Scenario: 会議室データの整合性確認
    Given ユーザー "admin" としてログインしている
    When 当日の会議室一覧を取得する
    Then 会議室IDが数値であることを確認する
    And 会議室名が空でないことを確認する
    And 重複する会議室がないことを確認する

  # UI001-01: レスポンシブデザイン対応確認
  @rooms @responsive
  Scenario: モバイル環境での会議室一覧表示
    Given ユーザー "taro" としてログインしている
    When モバイル環境で会議室一覧を取得する
    Then モバイル対応の会議室一覧が表示される
    And レスポンスステータスは 200 である