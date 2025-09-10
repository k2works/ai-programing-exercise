# 要件定義 - 会議室予約システム

## システム価値

### システムコンテキスト

```plantuml
@startuml

title システムコンテキスト図 - 会議室予約システム

left to right direction

actor 会員
actor ゲスト
actor スタッフ
actor 施設運営者

agent 既存業務システム
agent メール配信システム

usecase 会議室予約システム
note top of 会議室予約システム
  会議室利用の完全セルフサービス化
  ・窓口問い合わせ80%削減
  ・会議室稼働率20%向上
  ・新規会員登録50%増加
  ・スタッフ単純作業時間70%削減
end note

:会員: -- (会議室予約システム)
:ゲスト: -- (会議室予約システム)
:スタッフ: -- (会議室予約システム)
:施設運営者: -- (会議室予約システム)
(会議室予約システム) -- 既存業務システム
(会議室予約システム) -- メール配信システム

@enduml
```

### 要求モデル

```plantuml
@startuml

title 要求モデル図 - 会議室予約システム

left to right direction

actor 会員
note "利用可能な会議室があるかをWebで検索したい" as m_r1
note "利用可能な会議室をWebで予約したい" as m_r2  
note "予約をWebでキャンセルしたい" as m_r6
note "サービス内容の問い合わせをしたい" as m_r7
note as m_dr1 #Turquoise
  会員への派生要求
  ・いつでも自分で会議室利用を完結
  ・予約完了まで90%時間短縮
  ・直感的で説明不要なUI
end note
:会員: -- m_r1
:会員: -- m_r2
:会員: -- m_r6
:会員: -- m_r7
m_r1 -- m_dr1
m_r2 -- m_dr1
m_r6 -- m_dr1
m_r7 -- m_dr1

actor スタッフ
note "会員の予約を管理したい" as s_r3
note "利用者を管理したい" as s_r4
note "会議室を管理したい" as s_r5
note as s_dr1 #Turquoise
  スタッフへの派生要求
  ・ルーチンワーク削減
  ・予約関連問い合わせ80%削減
  ・付加価値業務への集中
end note
:スタッフ: -- s_r3
:スタッフ: -- s_r4
:スタッフ: -- s_r5
s_r3 -- s_dr1
s_r4 -- s_dr1
s_r5 -- s_dr1

actor ゲスト
note "サービス内容の問い合わせをしたい" as g_r7
note as g_dr1 #Turquoise
  ゲストへの派生要求
  ・サービス詳細の明確化
  ・Web経由問い合わせ100%増加
end note
:ゲスト: -- g_r7
g_r7 -- g_dr1

@enduml
```

## システム外部環境

### ビジネスコンテキスト

```plantuml
@startuml

title ビジネスコンテキスト図 - 会議室予約システム

left to right direction

actor ゲスト
actor 潜在顧客

node 施設運営組織 {
  rectangle フロント部門 {
    actor スタッフ
    actor 管理者
  }
  
  rectangle 利用者部門 {
    actor 会員
  }
  
  usecase 会議室予約業務
  usecase 利用者管理業務
  usecase 問い合わせ対応業務
  
  artifact 予約台帳
  artifact 会員情報
}

node 外部組織 {
  agent 既存業務システム
  agent メール配信システム
}

:ゲスト: -- (問い合わせ対応業務)
:潜在顧客: -- (問い合わせ対応業務)

(会議室予約業務) -- :会員:
(会議室予約業務) -- :スタッフ:
(利用者管理業務) -- :スタッフ:
(利用者管理業務) -- :管理者:
(問い合わせ対応業務) -- :スタッフ:

(会議室予約業務) -- 予約台帳
(利用者管理業務) -- 会員情報

(会議室予約業務) -- 既存業務システム
(問い合わせ対応業務) -- メール配信システム

@enduml
```

### ビジネスユースケース

#### 会議室予約業務

```plantuml
@startuml

title ビジネスユースケース図 - 会議室予約業務

left to right direction

actor 会員
actor スタッフ

agent 施設_窓口

usecase 会議室検索 as uc_01
usecase 予約作成 as uc_02
usecase 予約確認 as uc_03
usecase 予約変更 as uc_04
usecase 予約キャンセル as uc_05

artifact 予約台帳 as af_01
artifact 会議室情報 as af_02
artifact 利用履歴 as af_03

:会員: -- (uc_01)
:会員: -- (uc_02)
:会員: -- (uc_03)
:会員: -- (uc_05)
:スタッフ: -- (uc_02)
:スタッフ: -- (uc_03)
:スタッフ: -- (uc_04)
:スタッフ: -- (uc_05)

(uc_01) -- 施設_窓口
(uc_02) -- 施設_窓口
(uc_03) -- 施設_窓口
(uc_04) -- 施設_窓口
(uc_05) -- 施設_窓口

(uc_01) -- af_02
(uc_02) -- af_01
(uc_02) -- af_02
(uc_03) -- af_01
(uc_04) -- af_01
(uc_05) -- af_01
(uc_05) -- af_03

@enduml
```

#### 利用者管理業務

```plantuml
@startuml

title ビジネスユースケース図 - 利用者管理業務

left to right direction

actor スタッフ
actor 管理者

agent 施設_窓口

usecase 会員登録 as uc_01
usecase 会員情報更新 as uc_02

artifact 会員情報 as af_01
artifact 利用履歴 as af_02

:スタッフ: -- (uc_01)
:管理者: -- (uc_01)
:管理者: -- (uc_02)

(uc_01) -- 施設_窓口
(uc_02) -- 施設_窓口

(uc_01) -- af_01
(uc_02) -- af_01
(uc_02) -- af_02

@enduml
```

### 業務フロー

#### 会議室検索・予約の業務フロー

```plantuml
@startuml

title 業務フロー図 - 会議室検索・予約

|会員|
partition 検索フェーズ {
  :利用条件決定;
  :空室検索実行;
  :候補会議室確認;
}

partition 予約フェーズ {
  :予約内容入力;
  :予約確定依頼;
  if (予約可能?) then (yes)
    :予約完了確認;
  else (no)
    -> 重複・制約エラー;
    :代替候補検討;
    :再検索実行;
  endif
}
stop

@enduml
```

#### 問い合わせ対応の業務フロー

```plantuml
@startuml

title 業務フロー図 - 問い合わせ対応

|ゲスト/会員|
start
:問い合わせ内容作成;
:フォーム送信;
:受付確認;

|スタッフ|
:問い合わせ確認;
if (対応可能?) then (yes)
  :回答作成;
  :対応完了連絡;
else (no)
  :担当者転送;
  :追加調査実施;
  :回答作成;
  :対応完了連絡;
endif
stop

@enduml
```

### 利用シーン

#### 会議室予約の利用シーン

```plantuml
@startuml

title 利用シーン図 - 会議室予約

left to right direction

actor 会員
actor スタッフ

frame 急な会議室ニーズ
note right of 急な会議室ニーズ
  背景: 急遽会議が必要になった
  目的: すぐに利用可能な会議室を予約したい
  期待効果: 窓口確認時間の短縮
  制約条件: 2時間前までの予約が必要
end note

frame 定期利用予約
note right of 定期利用予約
  背景: 毎週定期的に会議室を利用
  目的: 継続的に同一会議室を確保したい
  期待効果: 予約漏れ防止、業務効率化
  制約条件: 1会員3件まで同時予約可能
end note

usecase 会議室検索
usecase 即時予約
usecase 繰返し予約

:会員: -- 急な会議室ニーズ
:スタッフ: -- 急な会議室ニーズ
急な会議室ニーズ -- (会議室検索)
急な会議室ニーズ -- (即時予約)

:会員: -- 定期利用予約
定期利用予約 -- (繰返し予約)

@enduml
```

#### 問い合わせの利用シーン

```plantuml
@startuml

title 利用シーン図 - 問い合わせ

left to right direction

actor ゲスト
actor 会員

frame サービス詳細確認
note right of サービス詳細確認
  背景: 利用を検討している
  目的: サービス内容・料金を知りたい
  期待効果: 利用検討の判断材料取得
  制約条件: 営業時間外でも受付可能
end note

usecase サービス問い合わせ
usecase 利用相談

:ゲスト: -- サービス詳細確認
:会員: -- サービス詳細確認
サービス詳細確認 -- (サービス問い合わせ)
サービス詳細確認 -- (利用相談)

@enduml
```

### バリエーション・条件

#### 利用者分類

| 分類名 | 説明 |
|--------|------|
| ゲスト | 未登録の利用検討者、サービス詳細確認のみ可能 |
| 会員 | 登録済み有料利用者、全予約機能が利用可能 |
| スタッフ | 施設運営担当、管理機能が利用可能 |

#### 会議室分類

| 分類名 | 説明 |
|--------|------|
| 小会議室 | 定員1-4名、基本料金 |
| 中会議室 | 定員5-10名、標準料金 |
| 大会議室 | 定員11名以上、プレミアム料金 |

#### 予約状態分類

| 分類名 | 説明 |
|--------|------|
| 確定 | 正常に予約された状態 |
| キャンセル済み | 利用者によりキャンセルされた状態 |
| 強制キャンセル | システムまたはスタッフによりキャンセルされた状態 |

## システム境界

### ユースケース複合図

#### 会員向けユースケース複合図

```plantuml
@startuml

title ユースケース複合図 - 会員向け機能

left to right direction

actor 会員 as user

frame "急な会議室ニーズ" as f01
usecase "会議室検索" as UC1
usecase "会議室予約" as UC2
boundary "検索画面" as b01
boundary "予約画面" as b02
entity "会議室情報" as e01
entity "予約情報" as e02
control "重複チェック" as c01
control "時間制約チェック" as c02

user -- f01
f01 -- UC1
f01 -- UC2

b01 -- UC1
UC1 -- e01
UC1 -- c02

b02 -- UC2
UC2 -- e02
UC2 -- c01

frame "予約管理" as f02
usecase "予約確認" as UC3
usecase "予約キャンセル" as UC4
boundary "予約一覧画面" as b03
entity "利用履歴" as e03
control "キャンセル制約" as c03

user -- f02
f02 -- UC3
f02 -- UC4
b03 -- UC3
UC3 -- e03
b03 -- UC4
UC4 -- e02
UC4 -- c03

@enduml
```

#### スタッフ向けユースケース複合図

```plantuml
@startuml

title ユースケース複合図 - スタッフ向け機能

left to right direction

actor スタッフ as staff

frame "利用者管理業務" as f01
usecase "会員登録" as UC1
usecase "会員情報更新" as UC2
usecase "会員削除" as UC3
boundary "会員管理画面" as b01
entity "会員情報" as e01
entity "利用履歴" as e02
control "権限チェック" as c01

staff -- f01
f01 -- UC1
f01 -- UC2
f01 -- UC3

b01 -- UC1
b01 -- UC2
b01 -- UC3
UC1 -- e01
UC2 -- e01
UC3 -- e01
UC2 -- e02
UC3 -- e02
UC1 -- c01
UC2 -- c01
UC3 -- c01

frame "会議室管理業務" as f02
usecase "会議室登録" as UC4
usecase "会議室更新" as UC5
usecase "メンテナンス管理" as UC6
boundary "会議室管理画面" as b02
entity "会議室情報" as e03
control "予約影響確認" as c02

staff -- f02
f02 -- UC4
f02 -- UC5
f02 -- UC6

b02 -- UC4
b02 -- UC5
b02 -- UC6
UC4 -- e03
UC5 -- e03
UC6 -- e03
UC5 -- c02
UC6 -- c02

frame "問い合わせ対応業務" as f03
usecase "問い合わせ確認" as UC7
usecase "問い合わせ対応" as UC8
boundary "問い合わせ管理画面" as b03
entity "問い合わせ情報" as e04
interface "メール通知" as i01

staff -- f03
f03 -- UC7
f03 -- UC8
b03 -- UC7
b03 -- UC8
UC7 -- e04
UC8 -- e04
UC8 -- i01

@enduml
```

## システム

### 情報モデル

```plantuml
@startuml

title 情報モデル図 - 会議室予約システム

left to right direction

' 利用者関連エンティティ群
entity ユーザー情報
entity ログイン情報
entity 利用履歴

' 会議室関連エンティティ群
entity 会議室情報
entity 設備情報
entity 料金情報
entity メンテナンス情報

' 予約関連エンティティ群
entity 予約情報
entity 予約詳細
entity キャンセル情報

' 問い合わせ関連エンティティ群
entity 問い合わせ情報
entity 対応履歴

' 関連付け
ユーザー情報 -- ログイン情報
ユーザー情報 -- 利用履歴

会議室情報 -- 設備情報
会議室情報 -- 料金情報
会議室情報 -- メンテナンス情報

ユーザー情報 -- 予約情報
会議室情報 -- 予約情報
予約情報 -- 予約詳細
予約情報 -- キャンセル情報

利用履歴 -- 予約情報

ユーザー情報 -- 問い合わせ情報
問い合わせ情報 -- 対応履歴

@enduml
```

### 状態モデル

#### 予約情報の状態遷移

```plantuml
@startuml
[*] --> 初期

初期 --> 確定 : 会議室予約(UC2)

state 確定 {
  [*] --> アクティブ
  アクティブ --> 利用中 : 利用開始
  利用中 --> 利用完了 : 利用終了
}

確定 --> キャンセル済み : 予約キャンセル(UC4)
確定 --> 強制キャンセル : スタッフ強制キャンセル

キャンセル済み --> [*]
強制キャンセル --> [*]
確定 --> 完了 : 利用完了
完了 --> [*]
@enduml
```

#### 会議室情報の状態遷移

```plantuml
@startuml
title 会議室情報の状態遷移図

[*] --> 登録済み : 会議室登録(UC4)

登録済み --> 利用可能 : 運用開始
利用可能 --> 予約中 : 予約作成
予約中 --> 利用可能 : 予約完了・キャンセル
利用可能 --> メンテナンス中 : メンテナンス開始(UC6)
メンテナンス中 --> 利用可能 : メンテナンス完了(UC6)

利用可能 --> 停止 : 会議室更新(UC5)
予約中 --> 停止 : 会議室更新(UC5)
メンテナンス中 --> 停止 : 会議室更新(UC5)
停止 --> [*]
@enduml
```

#### 問い合わせ情報の状態遷移

```plantuml
@startuml
[*] --> 受付 : 問い合わせ送信(UC_submit)

受付 --> 対応中 : 問い合わせ確認(UC7)

state 対応中 {
  [*] --> 調査中
  調査中 --> 回答中 : 問い合わせ対応(UC8)
  回答中 --> 調査中 : 追加調査必要
}

対応中 --> 解決済み : 対応完了(UC8)
対応中 --> クローズ : 対応不要判断

解決済み --> [*]
クローズ --> [*]
@enduml
```
