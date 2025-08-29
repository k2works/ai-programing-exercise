---
title: テスト駆動開発から始めるC#入門 ~2時間でTDDとリファクタリングのエッセンスを体験する~
description: 
published: true
date: 2025-07-14T01:16:34.396Z
tags: 
editor: markdown
dateCreated: 2025-07-14T01:16:34.396Z
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

今回C#のテスティングフレームワークには [xUnit.net](https://xunit.net/)を利用します。xUnitの詳しい使い方に関しては [公式ドキュメント](https://xunit.net/#documentation) を参照してください。では、まず.NET環境をセットアップしてプロジェクトを作成しましょう。

まず、.NET SDKがインストールされているかを確認します。

``` bash
$ dotnet --version
8.0.412
```

.NET 9.0が利用できることが確認できました。続いて、FizzBuzzプロジェクトを作成します。

``` bash
$ dotnet new sln -n FizzBuzzCSharp
$ dotnet new classlib -n FizzBuzz
$ dotnet new xunit -n FizzBuzzTest
$ dotnet sln FizzBuzzCSharp.sln add FizzBuzz/FizzBuzz.csproj FizzBuzzTest/FizzBuzzTest.csproj
$ dotnet add FizzBuzzTest/FizzBuzzTest.csproj reference FizzBuzz/FizzBuzz.csproj
```

プロジェクト構成は以下のようになります：

```
app/
├── FizzBuzzCSharp.sln
├── FizzBuzz/
│   ├── FizzBuzz.csproj
│   └── Class1.cs
└── FizzBuzzTest/
    ├── FizzBuzzTest.csproj
    └── UnitTest1.cs
```

では、まず以下の内容のテストファイルを作成して `UnitTest1.cs` を更新します。

``` csharp
using Xunit;

namespace FizzBuzzTest;

public class HelloTest
{
    [Fact]
    public void Test_Greeting()
    {
        Assert.Equal("hello world", Greeting());
    }
    
    private string Greeting()
    {
        return "hello world";
    }
}
```

テストを実行します。

``` bash
$ dotnet test
復元対象のプロジェクトを決定しています...
  /workspaces/ai-programing-exercise/app/FizzBuzzTest/FizzBuzzTest.csproj を復元しました (326 ミリ秒)。
  2 個中 1 個の復元対象のプロジェクトは最新です。
  FizzBuzz -> /workspaces/ai-programing-exercise/app/FizzBuzz/bin/Debug/net8.0/FizzBuzz.dll
  FizzBuzzTest -> /workspaces/ai-programing-exercise/app/FizzBuzzTest/bin/Debug/net8.0/FizzBuzzTest.dll
/workspaces/ai-programing-exercise/app/FizzBuzzTest/bin/Debug/net8.0/FizzBuzzTest.dll (.NETCoreApp,Version=v8.0) のテスト実行
VSTest のバージョン 17.11.1 (x64)

テスト実行を開始しています。お待ちください...
合計 1 個のテスト ファイルが指定されたパターンと一致しました。

成功!   -失敗:     0、合格:     1、スキップ:     0、合計:     1、期間: < 1 ms - FizzBuzzTest.dll (net8.0)
```

テストは成功しましたね。では続いてテストを失敗させてみましょう。`hello world` を `hello world!!!` に書き換えてテストを実行してみるとどうなるでしょうか。

``` csharp
...
    [Fact]
    public void Test_Greeting()
    {
        Assert.Equal("hello world!!!", Greeting());
    }
...
```

テストが失敗することが確認できました。テスティングフレームワークが正常に読み込まれて動作することが確認できました。テストが正常に通るように戻しておきましょう。続いてバージョン管理システムのセットアップをしておきましょう。バージョン管理システム何それ？だって！？君はセーブしないでロールプレイングゲームをクリアできるのか？できないならまず [ここ](https://backlog.com/ja/git-tutorial/intro/01/)でGitを使ったバージョン管理の基本を学んでおきましょう。

``` bash
$ git add .
$ git commit -m 'test: セットアップ'
```

これで[ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)のうち **バージョン管理** と **テスティング** の準備が整いましたので **TODOリスト** の最初の作業に取り掛かかるとしましょう。

### 仮実装

TODOリスト

  - 数を文字列にして返す
    
      - **1を渡したら文字列"1"を返す**

  - 3 の倍数のときは数の代わりに｢Fizz｣と返す

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

**1を渡したら文字列"1"を返す** プログラムを `UnitTest1.cs` に書きましょう。最初に何を書くのかって？
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

まず、セットアッププログラムは不要なので削除しておきましょう。

``` csharp
using Xunit;

namespace FizzBuzzTest;
```

テストコードを書きます。え？日本語でテストケースを書くの？ですかって。開発体制にもよりますが日本人が開発するのであれば無理に英語で書くよりドキュメントとしての可読性が上がるのでテストコードであれば問題は無いと思います。

> テストコードを読みやすくするのは、テスト以外のコードを読みやすくするのと同じくらい大切なことだ。
> 
> —  リーダブルコード 

``` csharp
using Xunit;

namespace FizzBuzzTest;

public class FizzBuzzTest
{
    private FizzBuzz.FizzBuzz _fizzbuzz;

    public FizzBuzzTest()
    {
        _fizzbuzz = new FizzBuzz.FizzBuzz();
    }

    [Fact]
    public void Test_1を渡したら文字列1を返す()
    {
        Assert.Equal("1", _fizzbuzz.Generate(1));
    }
}
```

テストを実行します。

``` bash
$ dotnet test
# コンパイルエラーが発生
```

`FizzBuzz`クラスが定義されていない。そうですねまだ作ってないのだから当然ですよね。では`FizzBuzz.Generate` メソッドを作りましょう。どんな振る舞いを書けばいいのでしょうか？とりあえず最初のテストを通すために **仮実装** から始めるとしましょう。

> 仮実装を経て本実装へ
> 
> 失敗するテストを書いてから、最初に行う実装はどのようなものだろうか----ベタ書きの値を返そう。
> 
> —  テスト駆動開発 

`FizzBuzz` **クラス** を定義して **文字列リテラル** を返す `FizzBuzz.Generate` **インスタンスメソッド** を作成しましょう。

``` csharp
namespace FizzBuzz;

public class FizzBuzz
{
    public string Generate(int number)
    {
        return "1";
    }
}
```

テストが通ることを確認します。

``` bash
$ dotnet test
成功!   -失敗:     0、合格:     1、スキップ:     0、合計:     1、期間: < 1 ms
```

オッケー、これでTODOリストを片付けることができました。え？こんなベタ書きのプログラムでいいの？他に考えないといけないことたくさんあるんじゃない？ばかじゃないの？と思われるかもしませんが、この細かいステップに今しばらくお付き合いいただきたい。

TODOリスト

  - 数を文字列にして返す
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - **2を渡したら文字列"2"を返す**

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

<!-- end list -->

``` csharp
...
public class FizzBuzzTest
{
    private FizzBuzz.FizzBuzz _fizzbuzz;

    public FizzBuzzTest()
    {
        _fizzbuzz = new FizzBuzz.FizzBuzz();
    }

    [Fact]
    public void Test_1を渡したら文字列1を返す()
    {
        Assert.Equal("1", _fizzbuzz.Generate(1));
    }

    [Fact]
    public void Test_2を渡したら文字列2を返す()
    {
        Assert.Equal("2", _fizzbuzz.Generate(2));
    }
}
```

``` bash
$ dotnet test
# 失敗: Expected: "2", Actual: "1"
```

テストが失敗しました。それは文字列1しか返さないプログラムなのだから当然ですよね。では1が渡されたら文字列1を返し、2を渡したら文字列2を返すようにプログラムを修正しましょう。**数値** を **文字列** に変換する必要があります。.NETの公式ドキュメントで調べてみましょう。

.NETの公式ドキュメントは <https://docs.microsoft.com/> です。[数値の書式設定](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/formatting-types)から `ToString()` メソッドを使えばいいことがわかります。

``` csharp
namespace FizzBuzz;

public class FizzBuzz
{
    public string Generate(int number)
    {
        return number.ToString();
    }
}
```

テストを実行します。

``` bash
$ dotnet test
成功!   -失敗:     0、合格:     2、スキップ:     0、合計:     2、期間: < 1 ms
```

テストが無事通りました。このように２つ目のテストによって `FizzBuzz.Generate` メソッドの一般化を実現することができました。このようなアプローチを **三角測量** と言います。

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

``` csharp
...
  [Fact]
  public void Test_数字を渡したら文字列を返す()
  {
      Assert.Equal("1", _fizzbuzz.Generate(1));
      Assert.Equal("2", _fizzbuzz.Generate(2));
      Assert.Equal("3", _fizzbuzz.Generate(3));
      Assert.Equal("4", _fizzbuzz.Generate(4));
      Assert.Equal("5", _fizzbuzz.Generate(5));
  }
...
```

> テストの本質というのは、「こういう状況と入力から、こういう振る舞いと出力を期待する」のレベルまで要約できる。
> 
> —  リーダブルコード 

ここで一段落ついたので、これまでの作業内容をバージョン管理システムにコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'test: 数を文字列にして返す'
```

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

``` csharp
public class FizzBuzzTest
{
    [Fact]
    public void Test_1を渡したら文字列1を返す()
    {
        Assert.Equal("1", new FizzBuzz.FizzBuzz().Generate(1));
    }

    [Fact]
    public void Test_2を渡したら文字列2を返す()
    {
        Assert.Equal("2", new FizzBuzz.FizzBuzz().Generate(2));
    }
}
```

テストフレームワークでは前処理にあたる部分を実行する機能がサポートされています。xUnitでは **コンストラクタ** がそれに当たるので `FizzBuzz` オブジェクトを共有して共通利用できるようにしてみましょう。ここでは **フィールド** に `FizzBuzz` **クラス** の参照を **代入** して各テストメソッドで共有できるようにしました。

``` csharp
public class FizzBuzzTest
{
    private FizzBuzz.FizzBuzz _fizzbuzz;

    public FizzBuzzTest()
    {
        _fizzbuzz = new FizzBuzz.FizzBuzz();
    }

    [Fact]
    public void Test_1を渡したら文字列1を返す()
    {
        Assert.Equal("1", _fizzbuzz.Generate(1));
    }

    [Fact]
    public void Test_2を渡したら文字列2を返す()
    {
        Assert.Equal("2", _fizzbuzz.Generate(2));
    }
}
```

テストプログラムを変更してしまいましたが壊れていないでしょうか？確認するにはどうすればいいでしょう？ テストを実行して確認すればいいですよね。

``` bash
$ dotnet test
成功!   -失敗:     0、合格:     2、スキップ:     0、合計:     2、期間: < 1 ms
```

オッケー、前回コミットした時と同じグリーンの状態のままですよね。区切りが良いのでここでコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'refactor: メソッドの抽出'
```

#### 変数名の変更

もう一つ気になるところがあります。

``` csharp
...
public class FizzBuzz
{
    public string Generate(int number)
    {
        if (number % 3 == 0 && number % 5 == 0)
        {
            return "FizzBuzz";
        }
        if (number % 3 == 0)
        {
            return "Fizz";
        }
        if (number % 5 == 0)
        {
            return "Buzz";
        }
        return number.ToString();
    }
}
```

引数の名前が適切になっているか確認しましょう。`number`という名前は意味が明確で読みやすいコードになっています。

> コンパイラがわかるコードは誰にでも書ける。すぐれたプログラマは人間にとってわかりやすいコードを書く。
> 
> —  リファクタリング(第2版) 

> 名前は短いコメントだと思えばいい。短くてもいい名前をつければ、それだけ多くの情報を伝えることができる。
> 
> —  リーダブルコード 

この時点でテストコードとプロダクトコードを変更しましたがその変更はすでに作成した自動テストによって壊れていないことを簡単に確認することができました。え、こんな簡単な変更でプログラムが壊れるわけないじゃん、ドジっ子なの？ですって。残念ながら私は絶対ミスしない完璧な人間ではないし、どちらかといえば注意力の足りないプログラマなのでこんな間違いも普通にやらかします。

``` csharp
...
public class FizzBuzz
{
    public string Generate(int number)
    {
        return numbr.ToString(); // タイポ
    }
}
```

最初にプロダクトコードを書いて一通りの機能を作ってから動作を確認する進め方だとこの手の間違いはいつどこで作り込んだのかわからなくなるため原因の調査に時間がかかり残念な経験をしたドジっ子プログラマは変更なんてするもんじゃないと思いコードを変更することに不安を持つようになるでしょう。でも、テスト駆動開発ならそんなドジっ子プログラマでも自動テストと小さなステップのおかげで上記のようなしょうもない間違いもすぐに見つけてすぐに対応することができるのでコードを変更する勇気を持つことができるのです。

> テスト駆動開発は、プログラミング中の不安をコントロールする手法だ。
> 
> —  テスト駆動開発 

> リファクタリングでは小さなステップでプログラムを変更していく。そのため間違ってもバグを見つけるのは簡単である。
> 
> —  リファクタリング(第2版) 

このグリーンの状態にいつでも戻れるようにコミットして次の **TODOリスト** の内容に取り掛かるとしましょう。

``` bash
$ git add .
$ git commit -m 'refactor: 変数名の確認'
```

> リファクタリングが成功するたびにコミットしておけば、たとえ壊してしまったとしても、動いていた状態に戻すことができます。変更をコミットしておき、意味のある単位としてまとまってから、共有のリポジトリに変更をプッシュすればよいのです。
> 
> —  リファクタリング(第2版) 

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

``` csharp
...
    [Fact]
    public void Test_3を渡したら文字列Fizzを返す()
    {
        Assert.Equal("Fizz", _fizzbuzz.Generate(3));
    }
...
```

``` bash
$ dotnet test
# 失敗: Expected: "Fizz", Actual: "3"
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

``` csharp
public class FizzBuzz
{
    public string Generate(int number)
    {
        return number.ToString();
    }
}
```

ここでは **if文** と **演算子** を使ってみましょう。なんかプログラムっぽくなってきましたね。
3で割り切れる場合はFizzを返すということは **数値リテラル** 3で割った余りが0の場合は **文字列リテラル** Fizzを返すということなので余りを求める **演算子** を調べる必要がありますね。.NETの公式ドキュメントで **算術演算子** をキーワードで検索したところ [剰余演算子 %](https://docs.microsoft.com/ja-jp/dotnet/csharp/language-reference/operators/arithmetic-operators#remainder-operator-)を使えばいいことがわかりました。

``` csharp
public class FizzBuzz
{
    public string Generate(int number)
    {
        if (number % 3 == 0)
        {
            return "Fizz";
        }
        return number.ToString();
    }
}
```

テストを実行します。

``` bash
$ dotnet test
成功!   -失敗:     0、合格:     3、スキップ:     0、合計:     3、期間: < 1 ms
```

テストが通りました。続いて **5を渡したら文字列"Buzz"を返す** テストを追加しましょう。

``` csharp
...
    [Fact]
    public void Test_5を渡したら文字列Buzzを返す()
    {
        Assert.Equal("Buzz", _fizzbuzz.Generate(5));
    }
...
```

``` bash
$ dotnet test
# 失敗: Expected: "Buzz", Actual: "Fizz"
```

5の倍数の場合の処理を追加します。

``` csharp
public class FizzBuzz
{
    public string Generate(int number)
    {
        if (number % 3 == 0)
        {
            return "Fizz";
        }
        if (number % 5 == 0)
        {
            return "Buzz";
        }
        return number.ToString();
    }
}
```

テストを実行します。

``` bash
$ dotnet test
成功!   -失敗:     0、合格:     4、スキップ:     0、合計:     4、期間: < 1 ms
```

続いて **15を渡したら文字列"FizzBuzz"を返す** テストを追加しましょう。

``` csharp
...
    [Fact]
    public void Test_15を渡したら文字列FizzBuzzを返す()
    {
        Assert.Equal("FizzBuzz", _fizzbuzz.Generate(15));
    }
...
```

``` bash
$ dotnet test
# 失敗: Expected: "FizzBuzz", Actual: "Fizz"
```

3と5の両方の倍数の場合を考慮する必要があります。条件の順序に注意して実装しましょう。

``` csharp
public class FizzBuzz
{
    public string Generate(int number)
    {
        if (number % 3 == 0 && number % 5 == 0)
        {
            return "FizzBuzz";
        }
        if (number % 3 == 0)
        {
            return "Fizz";
        }
        if (number % 5 == 0)
        {
            return "Buzz";
        }
        return number.ToString();
    }
}
```

テストを実行します。

``` bash
$ dotnet test
成功!   -失敗:     0、合格:     5、スキップ:     0、合計:     5、期間: < 1 ms
```

すべてのテストが通りました。ここで作業をコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'feat: FizzBuzzの基本ロジックを実装'
```

最後に、1から100までの数をプリントする機能を追加しましょう。

まず、FizzBuzzプロジェクトをコンソールアプリケーションとして動作するように設定を変更します。

``` xml
<Project Sdk="Microsoft.NET.Sdk">

  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
    <ImplicitUsings>enable</ImplicitUsings>
    <Nullable>enable</Nullable>
  </PropertyGroup>

</Project>
```

そして、プログラムのエントリーポイントを作成します。

``` csharp
using System;
using FizzBuzz;

class Program
{
    static void Main(string[] args)
    {
        var fizzbuzz = new FizzBuzz.FizzBuzz();
        
        for (int i = 1; i <= 100; i++)
        {
            Console.WriteLine(fizzbuzz.Generate(i));
        }
    }
}
```

プログラムを実行してみましょう。

``` bash
$ cd FizzBuzz
$ dotnet run
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
...
```

完璧です！FizzBuzzアプリケーションが正常に動作しています。最後にこの変更もコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'feat: 1から100まで数をプリントする機能を追加'
```

### 学習用テスト

ここで少し休憩して、C#の言語機能について学習用テストを書いて理解を深めてみましょう。

> 学習用テスト
> 
> 外部APIの理解を深めるために、実際に使ってみるテストを書こう。
> 
> —  テスト駆動開発 

#### 配列とList<T>

C#では配列とジェネリックコレクションのList<T>の両方が利用できます。学習用テストを書いて動作を確認してみましょう。

``` csharp
[Fact]
public void Test_配列の動作()
{
    int[] numbers = { 1, 2, 3 };
    Assert.Equal(3, numbers.Length);
    Assert.Equal(1, numbers[0]);
    Assert.Equal(2, numbers[1]);
    Assert.Equal(3, numbers[2]);
}

[Fact]
public void Test_Listの動作()
{
    var numbers = new List<int> { 1, 2, 3 };
    Assert.Equal(3, numbers.Count);
    Assert.Equal(1, numbers[0]);
    Assert.Equal(2, numbers[1]);
    Assert.Equal(3, numbers[2]);
    
    numbers.Add(4);
    Assert.Equal(4, numbers.Count);
    Assert.Equal(4, numbers[3]);
}
```

#### 繰り返し処理

C#にはfor文、foreach文など複数の繰り返し構文があります。

``` csharp
[Fact]
public void Test_for文の動作()
{
    var result = new List<int>();
    for (int i = 1; i <= 3; i++)
    {
        result.Add(i);
    }
    Assert.Equal(new[] { 1, 2, 3 }, result);
}

[Fact]
public void Test_foreach文の動作()
{
    var numbers = new[] { 1, 2, 3 };
    var result = new List<int>();
    foreach (int number in numbers)
    {
        result.Add(number * 2);
    }
    Assert.Equal(new[] { 2, 4, 6 }, result);
}
```

#### メソッド呼び出し

C#でのメソッド呼び出しの動作を確認してみましょう。

``` csharp
[Fact]
public void Test_メソッド呼び出し()
{
    var calculator = new Calculator();
    Assert.Equal(5, calculator.Add(2, 3));
    Assert.Equal(1, calculator.Subtract(3, 2));
}

public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
    
    public int Subtract(int a, int b)
    {
        return a - b;
    }
}
```

### コードの不吉な臭い

現在のFizzBuzzコードを見直して、改善できる点がないか確認してみましょう。

> コードの不吉な臭いとは、リファクタリングが必要であることを示すヒントである。
> 
> —  リファクタリング(第2版) 

#### 不思議な名前

``` csharp
public class FizzBuzz
{
    public string Generate(int number)  // メソッド名は適切？
    {
        if (number % 3 == 0 && number % 5 == 0)
        {
            return "FizzBuzz";
        }
        if (number % 3 == 0)
        {
            return "Fizz";
        }
        if (number % 5 == 0)
        {
            return "Buzz";
        }
        return number.ToString();
    }
}
```

`Generate`は一般的すぎるかもしれません。もっと具体的な名前に変更することを検討できます。

``` csharp
public string ConvertToFizzBuzzString(int number)
```

または

``` csharp
public string Convert(int number)
```

#### 長い関数

現在のGenerateメソッドは短いですが、条件が複雑になってきています。条件を明確にするリファクタリングを考えてみましょう。

``` csharp
public class FizzBuzz
{
    public string Generate(int number)
    {
        if (IsFizzBuzz(number))
        {
            return "FizzBuzz";
        }
        if (IsFizz(number))
        {
            return "Fizz";
        }
        if (IsBuzz(number))
        {
            return "Buzz";
        }
        return number.ToString();
    }
    
    private bool IsFizzBuzz(int number)
    {
        return number % 3 == 0 && number % 5 == 0;
    }
    
    private bool IsFizz(int number)
    {
        return number % 3 == 0;
    }
    
    private bool IsBuzz(int number)
    {
        return number % 5 == 0;
    }
}
```

#### マジックナンバー

コード中の3や5は「マジックナンバー」と呼ばれます。意味を明確にするために定数として定義することができます。

``` csharp
public class FizzBuzz
{
    private const int FIZZ_DIVISOR = 3;
    private const int BUZZ_DIVISOR = 5;
    
    public string Generate(int number)
    {
        if (number % FIZZ_DIVISOR == 0 && number % BUZZ_DIVISOR == 0)
        {
            return "FizzBuzz";
        }
        if (number % FIZZ_DIVISOR == 0)
        {
            return "Fizz";
        }
        if (number % BUZZ_DIVISOR == 0)
        {
            return "Buzz";
        }
        return number.ToString();
    }
}
```

### 動作するきれいなコード

> 動作するきれいなコードを書く。これがTDDが目指すゴールだ。
> 
> —  テスト駆動開発 

#### ふりかえり

ここまでの作業を振り返ってみましょう。

**学んだこと:**

1. **テストファースト**: 実装前にテストを書くことで、何を作るべきかが明確になる
2. **仮実装**: まず動作するコードを書き、段階的に改善していく
3. **三角測量**: 複数のテストケースから一般化されたロジックを導き出す
4. **明白な実装**: シンプルな機能は直接実装する
5. **リファクタリング**: 動作を変えずに内部構造を改善する
6. **レッド・グリーン・リファクタリング**: TDDの基本サイクル

**TDDのメリット:**

- **自信**: テストがあることで変更に対する不安が軽減される
- **設計**: テストを先に書くことで、使いやすいAPIが設計される
- **ドキュメント**: テストが仕様書として機能する
- **デバッグ**: 問題の早期発見が可能
- **リファクタリング**: 安全に構造を改善できる

#### 良いコード

私たちが作成したFizzBuzzコードの良い点を確認してみましょう。

**テストコード:**

``` csharp
public class FizzBuzzTest
{
    private FizzBuzz.FizzBuzz _fizzbuzz;

    public FizzBuzzTest()
    {
        _fizzbuzz = new FizzBuzz.FizzBuzz();
    }

    [Fact]
    public void Test_1を渡したら文字列1を返す()
    {
        Assert.Equal("1", _fizzbuzz.Generate(1));
    }

    [Fact]
    public void Test_2を渡したら文字列2を返す()
    {
        Assert.Equal("2", _fizzbuzz.Generate(2));
    }

    [Fact]
    public void Test_3を渡したら文字列Fizzを返す()
    {
        Assert.Equal("Fizz", _fizzbuzz.Generate(3));
    }

    [Fact]
    public void Test_5を渡したら文字列Buzzを返す()
    {
        Assert.Equal("Buzz", _fizzbuzz.Generate(5));
    }

    [Fact]
    public void Test_15を渡したら文字列FizzBuzzを返す()
    {
        Assert.Equal("FizzBuzz", _fizzbuzz.Generate(15));
    }
}
```

**プロダクトコード:**

``` csharp
namespace FizzBuzz;

public class FizzBuzz
{
    public string Generate(int number)
    {
        if (number % 3 == 0 && number % 5 == 0)
        {
            return "FizzBuzz";
        }
        if (number % 3 == 0)
        {
            return "Fizz";
        }
        if (number % 5 == 0)
        {
            return "Buzz";
        }
        return number.ToString();
    }
}
```

**メインプログラム:**

``` csharp
using System;
using FizzBuzz;

class Program
{
    static void Main(string[] args)
    {
        var fizzbuzz = new FizzBuzz.FizzBuzz();
        
        for (int i = 1; i <= 100; i++)
        {
            Console.WriteLine(fizzbuzz.Generate(i));
        }
    }
}
```

**良いコードの特徴:**

1. **読みやすい**: 意図が明確に表現されている
2. **テスタブル**: 各機能が独立してテストできる
3. **単純**: 複雑さが最小限に抑えられている
4. **拡張可能**: 新しい要件にも対応しやすい構造

> きれいなコードとは、ほかの開発者が読みやすいコードである。
> 
> —  Clean Code 

> よいコードとは、読み手のことを考えて書かれたコードである。
> 
> —  リーダブルコード 

## 参考サイト

- [xUnit.net](https://xunit.net/) - C#のテスティングフレームワーク
- [.NET ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/) - .NETの公式ドキュメント
- [C# ガイド](https://docs.microsoft.com/ja-jp/dotnet/csharp/) - C#言語のガイド
- [テスト駆動開発 | Microsoft Docs](https://docs.microsoft.com/ja-jp/devops/develop/how-microsoft-develops-devops) - MicrosoftのTDDガイド

## 参考図書

- **テスト駆動開発** Kent Beck (オーム社)
- **リファクタリング(第2版)** Martin Fowler (オーム社)  
- **Clean Code** Robert C. Martin (KADOKAWA)
- **リーダブルコード** Dustin Boswell, Trevor Foucher (オライリージャパン)
- **Effective C# 6.0/7.0** Bill Wagner (翔泳社)
- **C#本格入門** 飯島 正 (技術評論社)
