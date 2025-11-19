# 実装計画

- [x] 1. プロジェクト設定と基盤構築





  - アプリケーションappディレクトリ直下に作成する
  - Gradleプロジェクトの初期化とbuild.gradleの設定
  - パッケージ構造の作成（ヘキサゴナルアーキテクチャ）
  - application.propertiesの設定（開発環境・本番環境）
  - _要件: すべて_

- [x] 2. データベース設計とマイグレーション





  - Flywayマイグレーションファイルの作成（スキーマ定義）
  - テストデータのマイグレーションファイル作成
  - データベース接続の確認
  - _要件: すべて_

- [x] 3. ドメインモデルの実装（認証ドメイン）
  - [x] 3.1 UserId値オブジェクトの実装
    - UserId.javaの作成
    - バリデーションロジックの実装
    - _要件: 1.1, 1.2_
  
  - [ ]* 3.2 UserIdのプロパティテストを作成
    - **プロパティ1: 有効な認証情報での認証成功**
    - **検証: 要件 1.1**
  
  - [x] 3.3 Password値オブジェクトの実装
    - Password.javaの作成
    - BCrypt暗号化判定メソッドの実装
    - _要件: 8.2_
  
  - [ ]* 3.4 Passwordのプロパティテストを作成
    - **プロパティ19: パスワードのBCrypt暗号化**
    - **検証: 要件 8.2**
  
  - [x] 3.5 Name値オブジェクトの実装
    - Name.javaの作成
    - フルネーム取得メソッドの実装
    - _要件: 1.1_
  
  - [x] 3.6 RoleName列挙型の実装
    - RoleName.javaの作成（ADMIN, USER）
    - _要件: 6.1_
  
  - [x] 3.7 Userエンティティの実装
    - User.javaの作成
    - isAdmin()メソッドの実装
    - _要件: 1.1, 6.1_
  
  - [x] 3.8 Userエンティティの単体テストを作成
    - ユーザー作成のテスト
    - 管理者判定のテスト
    - _要件: 1.1, 6.1_

- [x] 4. ドメインモデルの実装（会議室ドメイン）
  - [x] 4.1 RoomId値オブジェクトの実装
    - RoomId.javaの作成
    - _要件: 2.1, 2.2_
  
  - [x] 4.2 RoomName値オブジェクトの実装
    - RoomName.javaの作成
    - _要件: 2.1, 2.2_
  
  - [x] 4.3 MeetingRoomエンティティの実装
    - MeetingRoom.javaの作成
    - _要件: 2.1, 2.2_
  
  - [x] 4.4 会議室ドメインの単体テストを作成
    - 会議室作成のテスト
    - _要件: 2.1, 2.2_

- [x] 5. ドメインモデルの実装（予約ドメイン）
  - [x] 5.1 ReservationId値オブジェクトの実装
    - ReservationId.javaの作成
    - _要件: 3.1_
  
  - [x] 5.2 ReservedDate値オブジェクトの実装
    - ReservedDate.javaの作成
    - _要件: 3.1_
  
  - [x] 5.3 ReservationTimeSlot値オブジェクトの実装
    - ReservationTimeSlot.javaの作成
    - overlap()メソッドの実装（時間帯重複判定）
    - _要件: 3.2, 10.4_
  
  - [ ]* 5.4 ReservationTimeSlotのプロパティテストを作成
    - **プロパティ26: 時間枠重複検出アルゴリズム**
    - **検証: 要件 10.4**
  
  - [x] 5.5 ReservableRoomId値オブジェクトの実装
    - ReservableRoomId.javaの作成
    - _要件: 3.1_
  
  - [x] 5.6 ReservableRoomエンティティの実装
    - ReservableRoom.javaの作成
    - _要件: 3.1_
  
  - [x] 5.7 Reservationエンティティの実装
    - Reservation.javaの作成
    - overlap()メソッドの実装（予約重複判定）
    - _要件: 3.1, 3.2_
  
  - [ ]* 5.8 Reservationのプロパティテストを作成
    - **プロパティ7: 予約重複の検出と拒否**
    - **検証: 要件 3.2**
  
  - [x] 5.9 ReservationListコレクションの実装
    - ReservationList.javaの作成
    - _要件: 4.1, 4.2_
  
  - [x] 5.10 ReservableRoomListコレクションの実装
    - ReservableRoomList.javaの作成
    - _要件: 2.2_

- [x] 6. ポートインターフェースの定義
  - [x] 6.1 入力ポート（ユースケース）の定義
    - ReservationUseCase.javaの作成
    - RoomUseCase.javaの作成
    - _要件: すべて_
  
  - [x] 6.2 出力ポート（リポジトリ）の定義
    - ReservationPort.javaの作成
    - ReservableRoomPort.javaの作成
    - MeetingRoomPort.javaの作成
    - UserPort.javaの作成
    - _要件: すべて_

- [x] 7. 永続化層の実装（出力アダプタ）
  - [x] 7.1 JPAエンティティの作成
    - UserEntity.javaの作成
    - MeetingRoomEntity.javaの作成
    - ReservableRoomEntity.javaの作成
    - ReservationEntity.javaの作成
    - _要件: すべて_
  
  - [x] 7.2 JPAリポジトリの作成
    - UserJpaRepository.javaの作成
    - MeetingRoomJpaRepository.javaの作成
    - ReservableRoomJpaRepository.javaの作成（悲観的ロック含む）
    - ReservationJpaRepository.javaの作成
    - _要件: すべて_
  
  - [x] 7.3 永続化アダプタの実装
    - UserPersistenceAdapter.javaの作成
    - MeetingRoomPersistenceAdapter.javaの作成
    - ReservableRoomPersistenceAdapter.javaの作成
    - ReservationPersistenceAdapter.javaの作成
    - ドメインモデルとJPAエンティティ間のマッパー実装
    - _要件: すべて_
  
  - [ ]* 7.4 永続化アダプタの統合テストを作成
    - @DataJpaTestを使用したリポジトリテスト
    - 悲観的ロックの動作確認
    - _要件: 3.6, 10.1_

- [x] 8. Spring Security設定の実装
  - [x] 8.1 WebSecurityConfigの作成
    - セキュリティフィルターチェーンの設定
    - パスワードエンコーダーの設定
    - フォームログインの設定
    - _要件: 1.1, 1.2, 1.3, 8.1, 8.3_
  
  - [ ]* 8.2 WebSecurityConfigのテストを作成
    - **プロパティ18: CSRFトークンの包含**
    - **プロパティ20: 保護リソースへのアクセス制御**
    - **検証: 要件 8.1, 8.3**
  
  - [x] 8.3 AuthUserDetailsの実装
    - AuthUserDetails.javaの作成（UserDetails実装）
    - _要件: 1.1_
  
  - [x] 8.4 AuthUserDetailsServiceの実装
    - AuthUserDetailsService.javaの作成（UserDetailsService実装）
    - _要件: 1.1, 1.2_
  
  - [x] 8.5 AuthUserDetailsServiceのテストを作成
    - **プロパティ1: 有効な認証情報での認証成功**
    - **プロパティ2: 無効な認証情報での認証拒否**
    - **検証: 要件 1.1, 1.2**

- [x] 9. アプリケーションサービスの実装
  - [x] 9.1 ReservationServiceの実装
    - ReservationService.javaの作成
    - reserve()メソッドの実装（悲観的ロック、重複チェック）
    - cancel()メソッドの実装（権限チェック）
    - findReservations()メソッドの実装
    - findOne()メソッドの実装
    - _要件: 3.1, 3.2, 3.3, 3.6, 4.1, 5.1, 5.2, 5.3_
  
  - [ ]* 9.2 ReservationServiceのプロパティテストを作成
    - **プロパティ6: 有効なパラメータでの予約作成**
    - **プロパティ7: 予約重複の検出と拒否**
    - **プロパティ8: 予約不可能な組み合わせの拒否**
    - **プロパティ10: 同時予約の排他制御**
    - **プロパティ14: 自分の予約のキャンセル**
    - **プロパティ15: 他人の予約キャンセルの拒否**
    - **プロパティ16: 管理者による任意の予約キャンセル**
    - **プロパティ25: 競合時のトランザクションロールバック**
    - **検証: 要件 3.1, 3.2, 3.3, 3.6, 5.1, 5.2, 5.3, 10.3**
  
  - [x] 9.3 RoomServiceの実装
    - RoomService.javaの作成
    - findReservableRooms()メソッドの実装
    - findMeetingRoom()メソッドの実装
    - _要件: 2.1, 2.2, 2.5_
  
  - [ ]* 9.4 RoomServiceのプロパティテストを作成
    - **プロパティ4: 日付指定での会議室一覧取得**
    - **プロパティ5: 会議室一覧のソート順**
    - **検証: 要件 2.2, 2.5**

- [x] 10. カスタムバリデーションの実装
  - [x] 10.1 @ThirtyMinutesUnitアノテーションとバリデータの実装
    - ThirtyMinutesUnit.javaの作成
    - ThirtyMinutesUnitValidator.javaの作成
    - _要件: 3.5, 9.3_
  
  - [x] 10.2 @ThirtyMinutesUnitのプロパティテストを作成
    - **プロパティ9: 30分単位時刻の検証**
    - **検証: 要件 3.5, 9.3**
  
  - [x] 10.3 @EndTimeMustBeAfterStartTimeアノテーションとバリデータの実装
    - EndTimeMustBeAfterStartTime.javaの作成
    - EndTimeMustBeAfterStartTimeValidator.javaの作成
    - _要件: 3.4, 9.4_
  
  - [x] 10.4 @EndTimeMustBeAfterStartTimeのプロパティテストを作成
    - **プロパティ23: 終了時刻の検証**
    - **検証: 要件 9.4**

- [ ] 11. Webフォームの実装
  - [ ] 11.1 ReservationFormの作成
    - ReservationForm.javaの作成
    - Bean Validationアノテーションの設定
    - カスタムバリデーションの適用
    - _要件: 3.1, 3.4, 3.5, 9.1, 9.2, 9.3, 9.4_
  
  - [ ]* 11.2 ReservationFormのテストを作成
    - **プロパティ21: 必須フィールドの検証**
    - **プロパティ22: フィールド固有エラーメッセージ**
    - **検証: 要件 9.1, 9.2**

- [ ] 12. Webコントローラーの実装
  - [ ] 12.1 LoginControllerの実装
    - LoginController.javaの作成
    - ログインフォーム表示エンドポイントの実装
    - _要件: 1.1, 1.2_
  
  - [ ] 12.2 RoomsControllerの実装
    - RoomsController.javaの作成
    - 会議室一覧表示エンドポイントの実装（当日・指定日）
    - _要件: 2.1, 2.2, 2.3, 2.4, 2.5_
  
  - [ ]* 12.3 RoomsControllerのテストを作成
    - @WebMvcTestを使用したコントローラーテスト
    - **プロパティ4: 日付指定での会議室一覧取得**
    - **プロパティ5: 会議室一覧のソート順**
    - **検証: 要件 2.1, 2.2, 2.5**
  
  - [ ] 12.4 ReservationsControllerの実装
    - ReservationsController.javaの作成
    - 予約フォーム表示エンドポイントの実装
    - 予約作成エンドポイントの実装
    - 予約キャンセルエンドポイントの実装
    - _要件: 3.1, 3.2, 3.3, 4.1, 4.2, 4.3, 4.4, 4.5, 5.1, 5.2, 5.3, 5.4_
  
  - [ ]* 12.5 ReservationsControllerのテストを作成
    - @WebMvcTestを使用したコントローラーテスト
    - **プロパティ11: 予約一覧の取得と表示**
    - **プロパティ12: 予約一覧のソート順**
    - **プロパティ13: 予約表示内容の完全性**
    - **プロパティ17: キャンセル後の状態更新**
    - **検証: 要件 4.1, 4.2, 4.3, 5.4**

- [ ] 13. Thymeleafテンプレートの実装
  - [ ] 13.1 ログイン画面テンプレートの作成
    - loginForm.htmlの作成
    - CSRFトークンの埋め込み
    - _要件: 1.1, 1.2, 8.1_
  
  - [ ] 13.2 会議室一覧画面テンプレートの作成
    - listRooms.htmlの作成
    - 日付ナビゲーションの実装
    - _要件: 2.1, 2.2, 2.3, 2.4_
  
  - [ ] 13.3 予約画面テンプレートの作成
    - reserveForm.htmlの作成
    - 予約フォームの実装
    - 予約一覧表示の実装
    - キャンセルボタンの条件表示
    - _要件: 3.1, 4.1, 4.2, 4.3, 4.4, 4.5, 5.1_
  
  - [ ] 13.4 共通レイアウトとCSSの作成
    - layout.htmlの作成
    - style.cssの作成
    - _要件: すべて_

- [ ] 14. 例外ハンドリングの実装
  - [ ] 14.1 カスタム例外クラスの作成
    - AlreadyReservedException.javaの作成
    - UnavailableReservationException.javaの作成
    - _要件: 3.2, 3.3_
  
  - [ ] 14.2 グローバル例外ハンドラの実装
    - @ControllerAdviceを使用したエラーハンドリング
    - _要件: 3.2, 3.3, 5.2_

- [ ] 15. チェックポイント - すべてのテストが通ることを確認
  - すべてのテストが通ることを確認し、問題があれば質問する

- [ ]* 16. エンドツーエンドテストの作成
  - ログイン→会議室検索→予約→キャンセルのシナリオテスト
  - 複数ユーザーの同時操作シナリオテスト
  - エラーケースのシナリオテスト
  - _要件: すべて_

- [ ]* 17. プロパティベーステストの追加実装
  - [ ]* 17.1 認証関連のプロパティテスト
    - **プロパティ3: セッション状態の維持**
    - **検証: 要件 1.4**
  
  - [ ]* 17.2 データ検証のプロパティテスト
    - **プロパティ24: 日付形式の検証**
    - **検証: 要件 9.5**

- [ ] 18. 最終チェックポイント - すべてのテストが通ることを確認
  - すべてのテストが通ることを確認し、問題があれば質問する
