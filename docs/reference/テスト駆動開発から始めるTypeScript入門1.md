---
title: テスト駆動開発から始めるTypeScript入門 ~2時間でTDDとリファクタリングのエッセンスを体験する~
description: TypeScriptとViteを使ってテスト駆動開発の基本を学ぶ
published: true
date: 2025-07-03T00:00:00.000Z
tags: 
editor: markdown
dateCreated: 2025-07-03T00:00:00.000Z
---

# エピソード1

## TODOリストから始めるテスト駆動開発

### TODOリスト

プログラムを作成するにあたってまず何をすればよいだろうか？私は、まず仕様の確認をして **TODOリスト** を作るところから始めます。

> TODOリスト
> 
> 何をテストすべきだろうか----着手する前に、必要になりそうなテストをリストに書き出しておこう。
> 
> —  テスト駆動開発 

仕様

    1 から 100 までの数をプリントするプログラムを書け。
    ただし 3 の倍数のときは数の代わりに｢Fizz｣と、5 の倍数のときは｢Buzz｣とプリントし、
    3 と 5 両方の倍数の場合には｢FizzBuzz｣とプリントすること。

仕様の内容をそのままプログラムに落とし込むには少しサイズが大きいようですね。なので最初の作業は仕様を **TODOリスト** に分解する作業から着手することにしましょう。仕様をどのようにTODOに分解していくかは [50分でわかるテスト駆動開発](https://channel9.msdn.com/Events/de-code/2017/DO03?ocid=player)の26分あたりを参考にしてください。

TODOリスト

  - 数を文字列にして返す

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

まず `数を文字列にして返す`作業に取り掛かりたいのですがまだプログラミング対象としてはサイズが大きいようですね。もう少し具体的に分割しましょう。

  - 数を文字列にして返す
    
      - 1を渡したら文字列"1"を返す

これならプログラムの対象として実装できそうですね。

## テストファーストから始めるテスト駆動開発

### テストファースト

最初にプログラムする対象を決めたので早速プロダクトコードを実装・・・ではなく **テストファースト** で作業を進めていきましょう。まずはプログラムを実行するための準備作業を進める必要がありますね。

> テストファースト
> 
> いつテストを書くべきだろうか----それはテスト対象のコードを書く前だ。
> 
> —  テスト駆動開発 

では、どうやってテストすればいいでしょうか？テスティングフレームワークを使って自動テストを書きましょう。

> テスト（名詞） どうやってソフトウェアをテストすればよいだろか----自動テストを書こう。
> 
> —  テスト駆動開発 

今回TypeScriptのテスティングフレームワークには [Vitest](https://vitest.dev/)を利用します。Vitestは高速で軽量なテストランナーで、TypeScriptとViteエコシステムとの統合が優れています。では、まずプロジェクトの作成から始めましょう。

まず、Viteを使ってTypeScriptプロジェクトを作成します。

``` bash
$ npm create vite@latest app -- --template vanilla-ts
```

作成されたプロジェクトディレクトリに移動して依存関係をインストールします。

``` bash
$ cd app
$ npm install
```

テスティングフレームワークVitestをインストールします。

``` bash
$ npm install -D vitest
```

`package.json`のscriptsセクションにテスト関連のスクリプトを追加します。

``` json
{
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "test": "vitest",
    "test:watch": "vitest --watch"
  }
}
```

Vitestの設定ファイル `vitest.config.ts` を作成します。

``` typescript
import { defineConfig } from 'vitest/config'

export default defineConfig({
  test: {
    environment: 'jsdom',
    globals: true,
  },
})
```

TypeScriptの設定ファイル `tsconfig.json` を更新してVitestの型定義を追加します。

``` json
{
  "compilerOptions": {
    // ...既存の設定...
    "types": ["vitest/globals"]
  },
  "include": ["src", "**/*.test.ts"]
}
```

では、以下の内容のテストファイルを作成して `src/FizzBuzz.test.ts` で保存します。

``` typescript
import { describe, it, expect, beforeEach } from 'vitest'
import { FizzBuzz } from './FizzBuzz'

describe('FizzBuzz', () => {
  let fizzBuzz: FizzBuzz

  beforeEach(() => {
    fizzBuzz = new FizzBuzz()
  })

  describe('数を文字列にして返す', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(fizzBuzz.generate(1)).toBe('1')
    })
  })
})
```

テストを実行します。

``` bash
$ npm test -- --run
```

おおっと！いきなりエラーが出てきましたね。でも落ち着いてください。まず最初にやることはエラーメッセージの内容を読むことです。ここではFizzBuzzクラスが見つからないというエラーが表示されているはずです。そうですねまだ作ってないのだから当然ですよね。

テストが失敗することを確認したら、次に `FizzBuzz` クラスを作成しましょう。

### 仮実装

TODOリスト

  - 数を文字列にして返す
    
      - **1を渡したら文字列"1"を返す**

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

**1を渡したら文字列"1"を返す** プログラムを `src/FizzBuzz.ts` に書きましょう。最初に何を書くのかって？
アサーションを最初に書きましょう。

> アサートファースト
> 
> いつアサーションを書くべきだろうか----最初に書こう
> 
>   - システム構築はどこから始めるべきだろうか。システム構築が終わったらこうなる、というストーリーを語るところからだ。
> 
>   - 機能はどこから書き始めるべきだろうか。コードが書き終わったらこのように動く、というテストを書くところからだ。
> 
>   - ではテストはどこから書き始めるべきだろうか。それはテストの終わりにパスすべきアサーションを書くところからだ。
> 
> —  テスト駆動開発 

テストコードを書きます。開発体制にもよりますが日本人が開発するのであれば無理に英語で書くよりドキュメントとしての可読性が上がるのでテストコードであれば問題は無いと思います。

> テストコードを読みやすくするのは、テスト以外のコードを読みやすくするのと同じくらい大切なことだ。
> 
> —  リーダブルコード 

テストは失敗するはずです。`FizzBuzz`が定義されていない。そうですねまだ作ってないのだから当然ですよね。では`FizzBuzz.generate` メソッドを作りましょう。どんな振る舞いを書けばいいのでしょうか？とりあえず最初のテストを通すために **仮実装** から始めるとしましょう。

> 仮実装を経て本実装へ
> 
> 失敗するテストを書いてから、最初に行う実装はどのようなものだろうか----ベタ書きの値を返そう。
> 
> —  テスト駆動開発 

`FizzBuzz` **クラス** を定義して **文字列リテラル** を返す `generate` **メソッド** を作成しましょう。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    return '1'
  }
}
```

テストが通ることを確認します。

``` bash
$ npm test -- --run
```

オッケー、これでTODOリストを片付けることができました。え？こんなベタ書きのプログラムでいいの？他に考えないといけないことたくさんあるんじゃない？ばかじゃないの？と思われるかもしませんが、この細かいステップに今しばらくお付き合いいただきたい。

TODOリスト

  - 数を文字列にして返す
    
      - **1を渡したら文字列"1"を返す**

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

### 三角測量

1を渡したら文字列1を返すようにできました。では、2を渡したらどうなるでしょうか？

TODOリスト

  - 数を文字列にして返す
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - **2を渡したら文字列"2"を返す**

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

``` typescript
describe('数を文字列にして返す', () => {
  it('1を渡したら文字列"1"を返す', () => {
    expect(fizzBuzz.generate(1)).toBe('1')
  })

  it('2を渡したら文字列"2"を返す', () => {
    expect(fizzBuzz.generate(2)).toBe('2')
  })
})
```

``` bash
$ npm test -- --run
```

テストが失敗しました。それは文字列1しか返さないプログラムなのだから当然ですよね。では1が渡されたら文字列1を返し、2を渡したら文字列2を返すようにプログラムを修正しましょう。TypeScriptでは数値を文字列に変換するのに `toString()` メソッドを使います。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    return number.toString()
  }
}
```

テストを実行します。

``` bash
$ npm test -- --run
```

テストが無事通りました。このように２つ目のテストによって `generate` メソッドの一般化を実現することができました。このようなアプローチを **三角測量** と言います。

> 三角測量
> 
> テストから最も慎重に一般化を引き出すやり方はどのようなものだろうか----２つ以上の例があるときだけ、一般化を行うようにしよう。
> 
> —  テスト駆動開発 

TODOリスト

  - **数を文字列にして返す**
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

たかが **数を文字列にして返す** プログラムを書くのにこんなに細かいステップを踏んでいくの？と思ったかもしれません。プログラムを書くということは細かいステップを踏んで行くことなのです。そして、細かいステップを踏み続けることが大切なことなのです。

> TDDで大事なのは、細かいステップを踏むことではなく、細かいステップを踏み続けられるようになることだ。
> 
> —  テスト駆動開発

あと、テストケースの内容がアサーション一行ですがもっと検証するべきことがあるんじゃない？と思うでしょう。検証したいことがあれば独立したテストケースを追加しましょう。このような書き方はよろしくありません。

``` typescript
it('数字を渡したら文字列を返す', () => {
  expect(fizzBuzz.generate(1)).toBe('1')
  expect(fizzBuzz.generate(2)).toBe('2')
  expect(fizzBuzz.generate(3)).toBe('3')
  expect(fizzBuzz.generate(4)).toBe('4')
  expect(fizzBuzz.generate(5)).toBe('5')
})
```

> テストの本質というのは、「こういう状況と入力から、こういう振る舞いと出力を期待する」のレベルまで要約できる。
> 
> —  リーダブルコード 

## リファクタリングから始めるテスト駆動開発

### リファクタリング

ここでテスト駆動開発の流れを確認しておきましょう。

> 1.  レッド：動作しない、おそらく最初のうちはコンパイルも通らないテストを１つ書く。
> 
> 2.  グリーン:そのテストを迅速に動作させる。このステップでは罪を犯してもよい。
> 
> 3.  リファクタリング:テストを通すために発生した重複をすべて除去する。
> 
> レッド・グリーン・リファクタリング。それがTDDのマントラだ。
> 
> —  テスト駆動開発 

コードはグリーンの状態ですが **リファクタリング** を実施していませんね。重複を除去しましょう。

> リファクタリング(名詞) 外部から見たときの振る舞いを保ちつつ、理解や修正が簡単になるように、ソフトウェアの内部構造を変化させること。
> 
> —  リファクタリング(第2版) 

> リファクタリングする(動詞) 一連のリファクタリングを適用して、外部から見た振る舞いの変更なしに、ソフトウェアを再構築すること。
> 
> —  リファクタリング(第2版)

#### メソッドの抽出

テストコードを見てください。テストを実行するにあたって毎回前準備を実行する必要があります。こうした処理は往々にして同じ処理を実行するものなので
**メソッドの抽出** を適用して重複を除去しましょう。

> メソッドの抽出
> 
> ひとまとめにできるコードの断片がある。
> 
> コードの断片をメソッドにして、それを目的を表すような名前をつける。
> 
> —  新装版 リファクタリング 

``` typescript
describe('FizzBuzz', () => {
  let fizzBuzz: FizzBuzz

  beforeEach(() => {
    fizzBuzz = new FizzBuzz()
  })

  describe('数を文字列にして返す', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(fizzBuzz.generate(1)).toBe('1')
    })

    it('2を渡したら文字列"2"を返す', () => {
      expect(fizzBuzz.generate(2)).toBe('2')
    })
  })
})
```

テストフレームワークでは前処理にあたる部分を実行する機能がサポートされています。Vitestでは `beforeEach` フックがそれに当たるので `FizzBuzz` オブジェクトを共有して共通利用できるようにしてみましょう。

テストプログラムを変更してしまいましたが壊れていないでしょうか？確認するにはどうすればいいでしょう？ テストを実行して確認すればいいですよね。

``` bash
$ npm test -- --run
```

オッケー、前回コミットした時と同じグリーンの状態のままですよね。

#### 変数名の変更

もう一つ気になるところがあります。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    return number.toString()
  }
}
```

引数の名前が分かりやすいですが、処理の内容を見るとさらに改善の余地があります。ここは **変数名の変更** を適用して人間にとって読みやすいコードにリファクタリングしましょう。

> コンパイラがわかるコードは誰にでも書ける。すぐれたプログラマは人間にとってわかりやすいコードを書く。
> 
> —  リファクタリング(第2版) 

> 名前は短いコメントだと思えばいい。短くてもいい名前をつければ、それだけ多くの情報を伝えることができる。
> 
> —  リーダブルコード 

現在のコードはすでに適切な変数名になっていますが、内部処理を改善してみましょう。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    return result
  }
}
```

続いて、変更で壊れていないかを確認します。

``` bash
$ npm test -- --run
```

オッケー、この時点でテストコードとプロダクトコードを変更しましたがその変更はすでに作成した自動テストによって壊れていないことを簡単に確認することができました。

### 明白な実装

次は **3を渡したら文字列"Fizz"** を返すプログラムに取り組むとしましょう。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す
    
      - **3を渡したら文字列"Fizz"を返す**

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

まずは、**テストファースト** **アサートファースト** で小さなステップで進めていくんでしたよね。

``` typescript
describe('3の倍数の場合', () => {
  it('3を渡したら文字列"Fizz"を返す', () => {
    expect(fizzBuzz.generate(3)).toBe('Fizz')
  })
})
```

``` bash
$ npm test -- --run
```

さて、失敗するテストを書いたので次はテストを通すためのプロダクトコードを書くわけですがどうしましょうか？　**仮実装**　でベタなコードを書きますか？実現したい振る舞いは`もし3を渡したらならば文字列Fizzを返す` です。英語なら `If number is 3, result is Fizz`といったところでしょうか。ここは **明白な実装** で片付けた方が早いでしょう。

> 明白な実装
> 
> シンプルな操作を実現するにはどうすればいいだろうか----そのまま実装しよう。
> 
> 仮実装や三角測量は、細かく細かく刻んだ小さなステップだ。だが、ときには実装をどうすべきか既に見えていることが。
> そのまま進もう。例えば先ほどのplusメソッドくらいシンプルなものを仮実装する必要が本当にあるだろうか。
> 普通は、その必要はない。頭に浮かんだ明白な実装をただ単にコードに落とすだけだ。もしもレッドバーが出て驚いたら、あらためてもう少し歩幅を小さくしよう。
> 
> —  テスト駆動開発 

ここでは **if文** と **剰余演算子** を使ってみましょう。なんかプログラムっぽくなってきましたね。
3で割で割り切れる場合はFizzを返すということは **数値リテラル** 3で割った余りが0の場合は **文字列リテラル** Fizzを返すということなので余りを求める **演算子** を使います。TypeScriptでは剰余演算子 `%` を使用します。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0) {
      result = 'Fizz'
    }
    
    return result
  }
}
```

``` bash
$ npm test -- --run
```

テストがグリーンになりました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3 の倍数のときは数の代わりに｢Fizz｣と返す~~
    
      - ~~3を渡したら文字列"Fizz"を返す~~

  - 5 の倍数のときは｢Buzz｣と返す
    
      - 5を渡したら文字列"Buzz"を返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

レッド・グリーンときたので次はリファクタリングですね。ここではすでに可読性の高いコードになっているので、特にリファクタリングは必要ありません。

だんだんとリズムに乗ってきました。ここはギアを上げて **明白な実装** で引き続き **TODOリスト** の内容を片付けていきましょう。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3の倍数のときは数の代わりに｢Fizz｣と返す~~
    
      - ~~3を渡したら文字列"Fizz"を返す~~

  - 5 の倍数のときは｢Buzz｣と返す
    
      - **5を渡したら文字列"Buzz"を返す**

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

**テストファースト** **アサートファースト** で最初に失敗するテストを書いて

``` typescript
describe('5の倍数の場合', () => {
  it('5を渡したら文字列"Buzz"を返す', () => {
    expect(fizzBuzz.generate(5)).toBe('Buzz')
  })
})
```

``` bash
$ npm test -- --run
```

**if/else if/else文** を使って条件分岐を追加しましょう。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }
    
    return result
  }
}
```

``` bash
$ npm test -- --run
```

テストが通りました。

次に、3と5の両方の倍数の場合のテストを追加します。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3の倍数のときは数の代わりに｢Fizz｣と返す~~
    
      - ~~3を渡したら文字列"Fizz"を返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~
    
      - ~~5を渡したら文字列"Buzz"を返す~~

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す
    
      - **15を渡したら文字列"FizzBuzz"を返す**

  - 1 から 100 までの数

  - プリントする

``` typescript
describe('3と5の両方の倍数の場合', () => {
  it('15を渡したら文字列"FizzBuzz"を返す', () => {
    expect(fizzBuzz.generate(15)).toBe('FizzBuzz')
  })
})
```

``` bash
$ npm test -- --run
```

テストが失敗しました。3と5の両方の倍数の場合の実装を追加します。条件の順番を変更して、最も具体的な条件から始めます。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0 && number % 5 === 0) {
      result = 'FizzBuzz'
    } else if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }
    
    return result
  }
}
```

``` bash
$ npm test -- --run
```

テストが通りました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3の倍数のときは数の代わりに｢Fizz｣と返す~~
    
      - ~~3を渡したら文字列"Fizz"を返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~
    
      - ~~5を渡したら文字列"Buzz"を返す~~

  - ~~3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す~~
    
      - ~~15を渡したら文字列"FizzBuzz"を返す~~

  - 1 から 100 までの数
    
      - **1から100までの数を配列で返す**

  - プリントする

次に、1から100までの数をプリントする機能を追加します。まず、配列を返すメソッドのテストを追加します。

``` typescript
describe('1から100までの数をプリントする', () => {
  it('1から100までの数を配列で返す', () => {
    const result = fizzBuzz.generateList(1, 100)
    expect(result).toHaveLength(100)
    expect(result[0]).toBe('1')
    expect(result[1]).toBe('2')
    expect(result[2]).toBe('Fizz')
    expect(result[4]).toBe('Buzz')
    expect(result[14]).toBe('FizzBuzz')
    expect(result[99]).toBe('Buzz')
  })
})
```

``` bash
$ npm test -- --run
```

`generateList`メソッドを実装します。

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0 && number % 5 === 0) {
      result = 'FizzBuzz'
    } else if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }
    
    return result
  }

  generateList(start: number, end: number): string[] {
    const result: string[] = []
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i))
    }
    return result
  }
}
```

``` bash
$ npm test -- --run
```

テストが通りました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3の倍数のときは数の代わりに｢Fizz｣と返す~~
    
      - ~~3を渡したら文字列"Fizz"を返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~
    
      - ~~5を渡したら文字列"Buzz"を返す~~

  - ~~3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す~~
    
      - ~~15を渡したら文字列"FizzBuzz"を返す~~

  - ~~1 から 100 までの数~~
    
      - ~~1から100までの数を配列で返す~~

  - プリントする
    
      - **Webページに結果を表示する**

最後に、Webページで結果を表示する機能を実装します。`src/main.ts`ファイルを更新します。

``` typescript
import './style.css'
import { FizzBuzz } from './FizzBuzz'

const fizzBuzz = new FizzBuzz()

document.querySelector<HTMLDivElement>('#app')!.innerHTML = `
  <div>
    <h1>FizzBuzz</h1>
    <div class="card">
      <button id="generateBtn" type="button">1から100までのFizzBuzzを生成</button>
    </div>
    <div id="result"></div>
  </div>
`

const generateBtn = document.querySelector<HTMLButtonElement>('#generateBtn')!
const resultDiv = document.querySelector<HTMLDivElement>('#result')!

generateBtn.addEventListener('click', () => {
  const results = fizzBuzz.generateList(1, 100)
  resultDiv.innerHTML = `
    <h2>結果:</h2>
    <div class="fizzbuzz-grid">
      ${results.map((value, index) => `
        <div class="fizzbuzz-item ${getFizzBuzzClass(value)}">
          <span class="number">${index + 1}</span>
          <span class="result">${value}</span>
        </div>
      `).join('')}
    </div>
  `
})

function getFizzBuzzClass(value: string): string {
  if (value === 'FizzBuzz') return 'fizzbuzz'
  if (value === 'Fizz') return 'fizz'
  if (value === 'Buzz') return 'buzz'
  return 'number'
}
```

また、`src/style.css`にFizzBuzz用のスタイルを追加します。

``` css
.fizzbuzz-grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(80px, 1fr));
  gap: 8px;
  margin-top: 2rem;
  max-width: 800px;
  margin-left: auto;
  margin-right: auto;
}

.fizzbuzz-item {
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 8px;
  border-radius: 4px;
  border: 1px solid #333;
  background-color: #1a1a1a;
  transition: all 0.2s ease;
}

.fizzbuzz-item:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);
}

.fizzbuzz-item.fizz {
  background-color: #2d4a2d;
  border-color: #4a7c4a;
}

.fizzbuzz-item.buzz {
  background-color: #4a2d2d;
  border-color: #7c4a4a;
}

.fizzbuzz-item.fizzbuzz {
  background-color: #4a4a2d;
  border-color: #7c7c4a;
}

.fizzbuzz-item .number {
  font-size: 0.8em;
  color: #888;
  margin-bottom: 4px;
}

.fizzbuzz-item .result {
  font-size: 1.1em;
  font-weight: bold;
}
```

開発サーバーを起動して結果を確認します。

``` bash
$ npm run dev
```

ブラウザで `http://localhost:5173/` を開いてボタンをクリックすると、1から100までのFizzBuzzの結果が美しいグリッドレイアウトで表示されます。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3の倍数のときは数の代わりに｢Fizz｣と返す~~
    
      - ~~3を渡したら文字列"Fizz"を返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~
    
      - ~~5を渡したら文字列"Buzz"を返す~~

  - ~~3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す~~
    
      - ~~15を渡したら文字列"FizzBuzz"を返す~~

  - ~~1 から 100 までの数~~
    
      - ~~1から100までの数を配列で返す~~

  - ~~プリントする~~
    
      - ~~Webページに結果を表示する~~

## 完成したコード

### FizzBuzzクラス (src/FizzBuzz.ts)

``` typescript
export class FizzBuzz {
  generate(number: number): string {
    let result = number.toString()
    
    if (number % 3 === 0 && number % 5 === 0) {
      result = 'FizzBuzz'
    } else if (number % 3 === 0) {
      result = 'Fizz'
    } else if (number % 5 === 0) {
      result = 'Buzz'
    }
    
    return result
  }

  generateList(start: number, end: number): string[] {
    const result: string[] = []
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i))
    }
    return result
  }
}
```

### テストコード (src/FizzBuzz.test.ts)

``` typescript
import { describe, it, expect, beforeEach } from 'vitest'
import { FizzBuzz } from './FizzBuzz'

describe('FizzBuzz', () => {
  let fizzBuzz: FizzBuzz

  beforeEach(() => {
    fizzBuzz = new FizzBuzz()
  })

  describe('数を文字列にして返す', () => {
    it('1を渡したら文字列"1"を返す', () => {
      expect(fizzBuzz.generate(1)).toBe('1')
    })

    it('2を渡したら文字列"2"を返す', () => {
      expect(fizzBuzz.generate(2)).toBe('2')
    })
  })

  describe('3の倍数の場合', () => {
    it('3を渡したら文字列"Fizz"を返す', () => {
      expect(fizzBuzz.generate(3)).toBe('Fizz')
    })
  })

  describe('5の倍数の場合', () => {
    it('5を渡したら文字列"Buzz"を返す', () => {
      expect(fizzBuzz.generate(5)).toBe('Buzz')
    })
  })

  describe('3と5の両方の倍数の場合', () => {
    it('15を渡したら文字列"FizzBuzz"を返す', () => {
      expect(fizzBuzz.generate(15)).toBe('FizzBuzz')
    })
  })

  describe('1から100までの数をプリントする', () => {
    it('1から100までの数を配列で返す', () => {
      const result = fizzBuzz.generateList(1, 100)
      expect(result).toHaveLength(100)
      expect(result[0]).toBe('1')
      expect(result[1]).toBe('2')
      expect(result[2]).toBe('Fizz')
      expect(result[4]).toBe('Buzz')
      expect(result[14]).toBe('FizzBuzz')
      expect(result[99]).toBe('Buzz')
    })
  })
})
```

## まとめ

TypeScriptとViteを使用したテスト駆動開発によるFizzBuzzの実装を通じて、以下のことを学びました：

### テスト駆動開発の基本サイクル

1. **レッド**: 失敗するテストを書く
2. **グリーン**: テストを通すための最小限の実装
3. **リファクタリング**: コードの改善

### TDDの重要な技法

- **テストファースト**: プロダクトコードを書く前にテストを書く
- **アサートファースト**: テストの最初にアサーションを書く
- **仮実装**: 最初はベタ書きの値でテストを通す
- **三角測量**: 複数の例がある時だけ一般化する
- **明白な実装**: シンプルな場合は直接実装する

### リファクタリングの技法

- **メソッドの抽出**: 重複するコードをメソッドに抽出
- **変数名の変更**: より読みやすい名前に変更

### TypeScriptとViteの利点

- **型安全性**: TypeScriptによる開発時エラーの早期発見
- **高速テスト**: Vitestによる高速なテスト実行
- **モダンな開発環境**: Viteによる高速なビルドとHMR
- **優れた開発体験**: 豊富なツールサポートと統合

> テスト駆動開発は、プログラミング中の不安をコントロールする手法だ。
> 
> —  テスト駆動開発 

この小さなプログラムを通じて、テスト駆動開発の基本的な流れを体験することができました。実際のプロジェクトでもこの手法を活用することで、品質の高いソフトウェアを継続的に開発することができるでしょう。
