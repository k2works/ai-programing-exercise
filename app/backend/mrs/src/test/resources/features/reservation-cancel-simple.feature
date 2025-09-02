Feature: 予約キャンセル機能（簡易版）

  Scenario: 本人による予約キャンセル
    Given ユーザー "taro" としてログインしている
    When 予約を作成する:
      | roomId | 1          |
      | date   | 2025-09-03 |
      | start  | 14:00      |
      | end    | 15:00      |
    And 作成した予約をキャンセルする
    Then レスポンスステータスは 204 である