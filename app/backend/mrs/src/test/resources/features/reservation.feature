Feature: 予約機能（F003）・予約キャンセル機能（F004）

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

    Given 以下の予約可能時間帯が設定されている:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 09:00     | 18:00   |
      | 2      | 2025-09-03 | 09:00     | 18:00   |

  # F003-01: 予約作成機能
  @reservation @create
  Scenario: 一般ユーザーが会議室の予約を作成する
    Given ユーザー "taro" としてログインしている
    When 以下の予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 10:00     | 11:00   |
    Then 予約が正常に作成される
    And レスポンスステータスは 200 である

  @reservation @create
  Scenario: 管理者が会議室の予約を作成する
    Given ユーザー "admin" としてログインしている
    When 以下の予約を作成する:
      | roomId | date       | startTime | endTime |
      | 2      | 2025-09-03 | 14:00     | 15:30   |
    Then 予約が正常に作成される
    And レスポンスステータスは 200 である

  @reservation @create @validation
  Scenario: 30分単位での予約作成
    Given ユーザー "taro" としてログインしている
    When 以下の予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 09:30     | 10:00   |
    Then 予約が正常に作成される
    And レスポンスステータスは 200 である

  # F003-02: 予約状況表示機能
  @reservation @view
  Scenario: 会議室の予約状況を確認する
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 10:00     | 11:00   |
      | hanako | 1      | 2025-09-03 | 14:00     | 15:00   |
    When 会議室 1 の "2025-09-03" の予約状況を取得する
    Then 以下の予約が表示される:
      | userId | startTime | endTime |
      | taro   | 10:00     | 11:00   |
      | hanako | 14:00     | 15:00   |
    And レスポンスステータスは 200 である

  @reservation @view
  Scenario: キャンセルボタンの権限制御表示
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 10:00     | 11:00   |
      | hanako | 1      | 2025-09-03 | 14:00     | 15:00   |
    When 会議室 1 の "2025-09-03" の予約状況を取得する
    Then 自分の予約にのみキャンセルボタンが表示される
    And レスポンスステータスは 200 である

  @reservation @view @admin
  Scenario: 管理者が全予約のキャンセルボタンを表示
    Given ユーザー "admin" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 10:00     | 11:00   |
      | hanako | 1      | 2025-09-03 | 14:00     | 15:00   |
    When 会議室 1 の "2025-09-03" の予約状況を取得する
    Then 全ての予約にキャンセルボタンが表示される
    And レスポンスステータスは 200 である

  # F003-03: 入力値検証機能
  @reservation @validation @error
  Scenario: 開始時刻・終了時刻の必須チェック
    Given ユーザー "taro" としてログインしている
    When 以下の無効な予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 |           | 11:00   |
    Then バリデーションエラーが発生する
    And エラーメッセージ "開始時刻は必須です" が表示される

  @reservation @validation @error
  Scenario: 終了時刻が開始時刻より後であることの確認
    Given ユーザー "taro" としてログインしている
    When 以下の無効な予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 11:00     | 10:00   |
    Then バリデーションエラーが発生する
    And エラーメッセージ "終了時刻は開始時刻より後である必要があります" が表示される

  @reservation @validation @error
  Scenario: 30分単位以外の時刻入力エラー
    Given ユーザー "taro" としてログインしている
    When 以下の無効な予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 10:15     | 11:15   |
    Then バリデーションエラーが発生する
    And エラーメッセージに "30分単位" が含まれる

  # F005-02: 重複予約検出
  @reservation @conflict @error
  Scenario: 重複する時間帯での予約作成時のエラー
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | hanako | 1      | 2025-09-03 | 10:00     | 11:00   |
    When 以下の重複する予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 10:30     | 11:30   |
    Then 重複予約エラーが発生する
    And エラーメッセージ "入力の時間帯はすでに予約済みです。" が表示される

  @reservation @conflict @error
  Scenario: 完全重複する時間帯での予約作成エラー
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | hanako | 1      | 2025-09-03 | 10:00     | 11:00   |
    When 以下の重複する予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 10:00     | 11:00   |
    Then 重複予約エラーが発生する
    And エラーメッセージ "入力の時間帯はすでに予約済みです。" が表示される

  @reservation @conflict
  Scenario: 隣接する時間帯での予約作成（重複なし）
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | hanako | 1      | 2025-09-03 | 10:00     | 11:00   |
    When 以下の予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 11:00     | 12:00   |
    Then 予約が正常に作成される
    And レスポンスステータスは 200 である

  # F004-01: 予約キャンセル権限制御
  @cancellation @permission
  Scenario: 予約者本人が自分の予約をキャンセルする
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 10:00     | 11:00   |
    When 自分の予約をキャンセルする
    Then 予約が正常にキャンセルされる
    And レスポンスステータスは 200 である

  @cancellation @permission @admin
  Scenario: 管理者が他のユーザーの予約をキャンセルする
    Given ユーザー "admin" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 10:00     | 11:00   |
    When ユーザー "taro" の予約をキャンセルする
    Then 予約が正常にキャンセルされる
    And レスポンスステータスは 200 である

  @cancellation @permission @error
  Scenario: 一般ユーザーが他人の予約をキャンセルしようとする
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | hanako | 1      | 2025-09-03 | 10:00     | 11:00   |
    When ユーザー "hanako" の予約をキャンセルしようとする
    Then 権限エラーが発生する
    And レスポンスステータスは 403 である

  # F004-02: キャンセル実行機能
  @cancellation @execution
  Scenario: 存在する予約のキャンセル実行
    Given ユーザー "taro" としてログインしている
    Given 以下の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 10:00     | 11:00   |
    When 予約ID指定でキャンセル実行する
    Then 予約データが削除される
    And キャンセル成功メッセージが表示される

  @cancellation @execution @error
  Scenario: 存在しない予約のキャンセル実行
    Given ユーザー "taro" としてログインしている
    When 存在しない予約IDでキャンセル実行する
    Then 予約が見つからないエラーが発生する
    And エラーメッセージ "指定された予約が見つかりません" が表示される

  # N001-01: 性能要件 - 予約処理3秒以内
  @reservation @performance
  Scenario: 予約処理の応答時間要件
    Given ユーザー "taro" としてログインしている
    When 以下の予約を作成する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 15:00     | 16:00   |
    Then 応答時間は 3秒以内である
    And 予約が正常に作成される

  # C002: ビジネス制約 - 予約不可日での予約作成
  @reservation @constraint @error
  Scenario: 予約不可日での予約作成エラー
    Given ユーザー "taro" としてログインしている
    When 予約不可日 "2025-12-31" で予約を作成する:
      | roomId | startTime | endTime |
      | 1      | 10:00     | 11:00   |
    Then 予約不可エラーが発生する
    And エラーメッセージ "入力の日付・部屋の組合わせは予約できません。" が表示される

  # N003: セキュリティ要件 - 認証なしでのアクセス
  @reservation @security @error
  Scenario: 認証なしでの予約作成アクセス
    When 認証なしで予約作成をアクセスする
    Then 認証エラーが発生する
    And レスポンスステータスは 401 である

  @reservation @security @error
  Scenario: 認証なしでの予約キャンセルアクセス
    When 認証なしで予約キャンセルをアクセスする
    Then 認証エラーが発生する
    And レスポンスステータスは 401 である

  # F005-01: 悲観的ロック制御（同時予約防止）
  @reservation @concurrency
  Scenario: 同時予約による競合状態の防止
    Given ユーザー "taro" としてログインしている
    Given ユーザー "hanako" としてログインしている
    When 2つのセッションで同じ時間帯に同時予約を実行する:
      | roomId | date       | startTime | endTime |
      | 1      | 2025-09-03 | 16:00     | 17:00   |
    Then 1つの予約のみが成功する
    And もう一方の予約は重複エラーになる

  # データ整合性確認
  @reservation @data
  Scenario: 予約データの整合性確認
    Given ユーザー "admin" としてログインしている
    Given 複数の予約が存在する:
      | userId | roomId | date       | startTime | endTime |
      | taro   | 1      | 2025-09-03 | 09:00     | 10:00   |
      | hanako | 2      | 2025-09-03 | 10:00     | 11:30   |
      | admin  | 1      | 2025-09-03 | 14:00     | 15:00   |
    When 全予約データを取得する
    Then 予約IDが一意であることを確認する
    And 時間データが正しい形式であることを確認する
    And ユーザーIDと会議室IDが有効であることを確認する