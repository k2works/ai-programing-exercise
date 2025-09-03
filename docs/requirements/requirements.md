# 要件定義

## システム価値

### システムコンテキスト

```plantuml
@startuml

title システムコンテキスト図

left to right direction

actor 利用者 as ac_01
actor スタッフ as ac_02
actor 会員 as ac_03
actor ゲスト as ac_04
ac_01 <|- ac_02
ac_01 <|-- ac_03

note top of ac_01
  利用者とはシステムを利用するアクター全般を指す。
  お金を払って会議室を利用する会員。
  窓口業務を担当するスタッフ。
end note

note bottom of ac_04
    ゲストとはシステムの公開サービスを利用するアクターを指す。
    会議室予約に興味を持っている潜在顧客。
end note

usecase 会議室予約システム as uc_01
note top of uc_01
  利用者がWebサイト上で会議室の予約をして、一覧で確認できるようにする。
  登録した利用者だけが利用できるようにする。
  スタッフは利用者の予約を取り消すことができるようにする。
end note

ac_02 -- (uc_01)
ac_03 -- (uc_01)
ac_04 -- (uc_01)

@enduml
```

### 要求モデル

```plantuml
@startuml

title 要求モデル図

left to right direction

actor 会員 as ac_01
note "利用可能な会議室があるかを Web で検索したい" as k_r1
note "利用可能な会議室を Web で予約したい" as k_r2
note "予約を Web でキャンセルしたい" as k_r6
note "サービス内容の問い合わせをしたい" as k_r7
note as k_dr1 #Turquoise
  Web で会議室を検索でき、
  そのまま予約できること
end note
ac_01 -- k_r1
ac_01 -- k_r2
ac_01 -- k_r6
ac_01 -- k_r7
k_r1 -- k_dr1
k_r2 -- k_dr1
k_r6 -- k_dr1

actor スタッフ as ac_02
note "会員の予約を管理したい" as k_r3
note "利用者を管理したい" as k_r4
note "会議室を管理したい" as k_r5
ac_02 -- k_r3
ac_02 -- k_r4
ac_02 -- k_r5

actor ゲスト as ac_03
ac_03 -- k_r7

@enduml
```

## システム外部環境

### ビジネスコンテキスト

```plantuml
@startuml

title ビジネスコンテキスト図

left to right direction

actor 会員 as ac_01
actor ゲスト as ac_03

node 施設 {
  rectangle 窓口 {
    actor スタッフ as ac_02
  }
  usecase 予約管理 as uc_01
  usecase 会員管理 as uc_02
  artifact 会議室 as ar_01
}

ac_01 -- (uc_01)
ac_01 -- (uc_02)
ac_03 -- (uc_02)
(uc_01) -- (ar_01)
(uc_01) - ac_02
(uc_02) - ac_02

@enduml
```

### ビジネスユースケース

#### 会員管理

```plantuml
@startuml

title ビジネスユースケース図 - 会員管理

left to right direction

actor 会員 as ac_01
actor ゲスト as ac_02
agent 窓口 as ag_01

usecase 利用者の管理 as uc_01
usecase 利用者の認証 as uc_02
usecase 問い合わせ管理 as uc_03

ac_01 -- (uc_02)
ac_01 -- (uc_03)
ac_02 -- (uc_03)

(uc_01) -- ag_01
(uc_02) -- ag_01
(uc_03) -- ag_01

@enduml
```

#### 予約管理

```plantuml
@startuml

title ビジネスユースケース図 - 予約管理

left to right direction

actor 会員 as ac_01

agent 窓口 as ag_01

usecase 会議室の検索 as uc_01
usecase 会議室の予約 as uc_02
usecase 会議室の登録 as uc_03

artifact 会議室 as af_01

ac_01 -- (uc_01)
ac_01 -- (uc_02)

(uc_01) --- af_01
(uc_01) -- ag_01
(uc_02) -- af_01
(uc_02) -- ag_01
(uc_03) -- ag_01

@enduml
```

### 業務フロー

#### 会員管理

##### 問い合わせ管理

```plantuml
@startuml

title 業務フロー図 - 問い合わせ管理:BUC

|会員|
partition 問い合わせの実施 {
  :問い合わせをする;
}

|スタッフ|
partition 問い合わせの対応 {
  :問い合わせを確認する;
  :問い合わせに対応する;
}

|会員|
stop

@enduml
```

```plantuml
@startuml

title 業務フロー図 - 問い合わせ管理:BUC

|ゲスト|
partition 問い合わせの実施 {
  :問い合わせをする;
}

|スタッフ|
partition 問い合わせの対応 {
  :問い合わせを確認する;
  :問い合わせに対応する;
}

|ゲスト|
stop

@enduml
```

#### 予約管理

##### 会議室の予約

```plantuml
@startuml

title 業務フロー図 - 会議室の予約:BUC

|スタッフ|
partition 会議室の登録 {
  :会議室を登録する;
  :予約可能な会議室を登録する;
}

|会員|
partition 会議室の検索 {
  :予約可能な会議室を検索する;
}

partition 会議室の予約 {
  :会議室を予約する;
   if (キャンセル) then (必要)
    :取り消したい会議室の予約を選択する;
    :会議室の予約を取り消す;
   else (不要)
   endif
}

stop

@enduml
```

### 利用シーン

#### 会員管理

##### 利用者の認証

```plantuml
@startuml

title 利用シーン図 - 利用者の認証:BUC

left to right direction

actor 会員 as ac_01
actor スタッフ as ac_02

frame 利用者の認証
note right of 利用者の認証
  申し込み済みの利用者を認証して会議室の予約・取り消しを実行できるようにする
end note

usecase 利用者を認証する

:ac_01: -- 利用者の認証
:ac_02: -- 利用者の認証
利用者の認証 -- (利用者を認証する)

@enduml
```

##### 利用者の管理

```plantuml
@startuml

title 利用シーン図 - 利用者の管理:BUC

left to right direction

actor スタッフ as ac_02

frame 利用者の管理 as f_01
note right of f_01
 申し込みのあった利用者を窓口が手動でシステムに登録する
end note

usecase 利用申込みをする as uc_01
usecase 利用者を確認する as uc_02
usecase 利用者を登録をする as uc_03
usecase 利用者情報の更新をする as uc_04
usecase 利用者登録を抹消する as uc_05

:ac_02: -- f_01
f_01 -- (uc_01)
f_01 -- (uc_02)
f_01 -- (uc_03)
f_01 -- (uc_04)
f_01 -- (uc_05)

@enduml
```

#### 予約管理

##### 会議室の検索

```plantuml
@startuml

title 利用シーン図 - 会議室の検索:BUC

left to right direction

actor 会員 as ac_01
actor スタッフ as ac_02

frame 会議室の検索
note right of 会議室の検索
  利用者認証が完了している
  予約済みおよび予約可能な会議室を検索する
end note

usecase 予約可能な会議室を検索する

:ac_01: -- 会議室の検索
:ac_02: -- 会議室の検索
会議室の検索 -- (予約可能な会議室を検索する)


@enduml
```

##### 会議室の登録

```plantuml
@startuml

title 利用シーン図 - 会議室の登録:BUC

left to right direction

actor スタッフ as ac_01

frame 会議室の登録
note right of 会議室の登録
   予約可能な会議室を登録する。
   利用者は予約可能な会議室に対して予約をすることができる。
end note

usecase 予約可能な会議室を登録する

:ac_01: -- 会議室の登録
会議室の登録 -- (会議室を登録する)
会議室の登録 -- (予約可能な会議室を登録する)


@enduml
```

### バリエーション・条件

| 利用者区分 |
|-----------|
| 管理者     |
| 一般      |
| ゲスト     |

## システム境界

### ユースケース複合図

#### 会員管理

##### 利用者の認証

```plantuml
@startuml
actor "会員" as user
actor "スタッフ" as admin
frame "利用者の認証" as f01
usecase "利用者を認証する" as UC1
boundary "ログイン画面" as b01
entity "利用者" as e01
user - f01
admin - f01
f01 - UC1
b01 -- UC1
UC1 - e01
@enduml
```

##### 利用者の管理

```plantuml
@startuml
actor "スタッフ" as admin
frame "利用者の管理" as f01
usecase "利用者を確認する" as UC1
usecase "利用者を登録する" as UC2
usecase "利用者情報を更新する" as UC3
usecase "利用者登録を抹消する" as UC4
boundary "利用者一覧画面" as b01
boundary "利用者画面" as b02
entity "利用者" as e01
control "利用者バリエーション" as c01

admin - f01
f01 - UC1
f01 - UC2
f01 - UC3
f01 - UC4
b01 -- UC1
b02 -- UC2
b02 -- UC3
b02 -- UC4
UC2 -- c01
UC3 -- c01
UC4 -- c01
UC1 - e01
UC2 - e01
UC3 - e01
UC4 - e01
@enduml
```

##### 問い合わせ管理

```plantuml
@startuml
actor "ゲスト" as gust
actor "会員" as user
actor "スタッフ" as admin
frame "問い合わせの実施" as f01
frame "問い合わせの対応" as f02
usecase "問い合わせをする" as UC1
usecase "問い合わせを確認する" as UC2
usecase "問い合わせに対応する" as UC3
boundary "問い合わせ画面" as b01
boundary "問い合わせ一覧画面" as b02
entity "問い合わせ" as e01

gust - f01
user - f01
f01 - UC1
b01 -- UC1
UC1 - e01

f01 --> f02

admin - f02
f02 - UC2
f02 - UC3
b02 -- UC2
b02 -- UC3
UC2 -- e01
@enduml
```

#### 予約管理

##### 会議室の検索

```plantuml
@startuml
actor "会員" as user
actor "スタッフ" as admin
frame "会議室の検索" as f01
usecase "予約可能な会議室を検索する" as UC1
boundary "会議室予約一覧画面" as b01
entity "予約可能会議室" as e01

user - f01
admin - f01

f01 - UC1
b01 -- UC1
UC1 - e01

@enduml
```

##### 会議室の予約

```plantuml
@startuml
actor "会員" as user
actor "スタッフ" as admin
frame "会議室の予約" as f01
usecase "予約可能な会議室を検索する" as UC1
usecase "会議室の予約一覧を取得する" as UC2
usecase "会議室を予約する" as UC3
usecase "会議室の予約を取り消す" as UC4
boundary "会議室予約一覧画面" as b01
boundary "会議室予約画面" as b02
control "予約条件" as c01
entity "予約可能会議室" as e01
entity "予約" as e02

user - f01
admin - f01

f01 - UC1
b01 -- UC1
UC1 -- e01

f01 - UC2
b02 -- UC2
UC2 -- e02

f01 - UC3
b02 -- UC3
UC3 -- e02
UC3 -- c01

f01 - UC4
b02 -- UC4
UC4 - e02

@enduml
```

##### 会議室の登録

```plantuml
@startuml
actor "スタッフ" as admin
frame "会議室の登録" as f01
usecase "会議室を登録する" as UC1
usecase "予約可能な会議室を登録する" as UC2
boundary "会議室一覧画面" as b01
boundary "会議室登録画面" as b02
boundary "予約可能会議室一覧画面" as b03
boundary "予約可能会議室登録画面" as b04
control "予約可能条件" as c01
entity "予約可能会議室" as e01
entity "予約" as e02
entity "会議室" as e03

admin - f01
f01 - UC1
b01 -- UC1
b02 -- UC1
UC1 -- e03

f01 - UC2
b03 -- UC2
b04 -- UC2
UC2 -- e01
UC2 -- e02
UC2 -- e03
UC2 - c01

@enduml
```

## システム

### 情報モデル

```plantuml
@startuml

title 情報モデル図

left to right direction

entity 利用者
entity 予約
entity 会議室
entity 予約可能会議室

予約 -- 利用者
予約 -- 予約可能会議室
予約可能会議室 -- 会議室

@enduml
```

### 状態モデル

#### 予約の状態遷移

```plantuml
@startuml

title 予約の状態遷移図

[*] --> 予約要求
予約要求 --> 予約確定 : 空室確認OK
予約要求 --> 予約不可 : 満室・営業時間外
予約確定 --> 予約取消 : 取消要求
予約確定 --> [*] : 利用完了
予約取消 --> [*]
予約不可 --> [*]

note right of 予約要求
  利用者が会議室の予約を要求する状態
  - 日時・人数・会議室の指定
  - 空室状況の確認
  - 営業時間のチェック
end note

note right of 予約確定
  予約が正式に確定した状態
  - 予約IDが発行される
  - 予約一覧に表示される
  - 取消可能な状態
end note

note right of 予約取消
  予約が取り消された状態
  - 会議室が再び利用可能になる
  - 予約履歴に記録される
end note

@enduml
```

#### 利用者の状態遷移

```plantuml
@startuml

title 利用者の状態遷移図

[*] --> 未認証
未認証 --> 認証中 : ログイン要求
認証中 --> 認証済み : 認証成功
認証中 --> 認証失敗 : 認証エラー
認証済み --> 予約操作中 : 予約・検索・取消操作
予約操作中 --> 認証済み : 操作完了
認証済み --> 未認証 : ログアウト・セッション切れ
認証失敗 --> 未認証

note right of 未認証
  システムに未ログインの状態
  - 公開情報のみ閲覧可能
  - 問い合わせ可能
end note

note right of 認証済み
  システムにログイン済みの状態
  - 会議室の検索・予約が可能
  - 自分の予約一覧が確認可能
  - 予約の取消が可能
end note

note right of 予約操作中
  予約関連の操作を実行中の状態
  - 会議室の検索
  - 新規予約の作成
  - 既存予約の取消
end note

@enduml
```

#### 会議室の状態遷移

```plantuml
@startuml

title 会議室の状態遷移図

[*] --> 利用可能
利用可能 --> 予約済み : 予約確定
予約済み --> 利用中 : 利用開始時刻到達
利用中 --> 利用可能 : 利用終了時刻到達
予約済み --> 利用可能 : 予約取消
利用可能 --> メンテナンス中 : メンテナンス開始
メンテナンス中 --> 利用可能 : メンテナンス完了

note right of 利用可能
  会議室が予約可能な状態
  - 新規予約を受付可能
  - 検索結果に表示される
  - 営業時間内のみ有効
end note

note right of 予約済み
  会議室に予約が入っている状態
  - 該当時間帯は新規予約不可
  - 予約者のみ利用可能
  - 予約取消により利用可能に戻る
end note

note right of 利用中
  会議室が実際に使用されている状態
  - 新規予約不可
  - 利用時間終了で自動的に利用可能に戻る
end note

note right of メンテナンス中
  会議室がメンテナンス中の状態
  - 予約不可
  - 検索結果に表示されない
  - スタッフによる手動管理
end note

@enduml
```

#### システム全体の状態遷移

```plantuml
@startuml

title システム全体の状態遷移図

[*] --> 初期化中
初期化中 --> 正常稼働 : 起動完了
正常稼働 --> 高負荷状態 : アクセス集中
高負荷状態 --> 正常稼働 : 負荷軽減
正常稼働 --> メンテナンスモード : 計画メンテナンス
メンテナンスモード --> 正常稼働 : メンテナンス完了
正常稼働 --> 異常状態 : システム障害
異常状態 --> 正常稼働 : 障害復旧
異常状態 --> [*] : システム停止

note right of 正常稼働
  システムが正常に稼働している状態
  - 全機能が利用可能
  - 応答時間が正常範囲内
  - データベース接続正常
end note

note right of 高負荷状態
  システムに高い負荷がかかっている状態
  - 応答時間が遅延
  - 新規予約の制限を検討
  - 負荷分散の実施
end note

note right of メンテナンスモード
  計画的なメンテナンスを実施中の状態
  - 新規予約を停止
  - 既存予約の確認のみ可能
  - システム更新・データ整備
end note

@enduml
```



