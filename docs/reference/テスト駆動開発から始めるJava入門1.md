---
title: テスト駆動開発から始めるJava入門 ~2時間でTDDとリファクタリングのエッセンスを体験する~
description: 
published: true
date: 2025-07-02T10:00:00.000Z
tags: 
editor: markdown
dateCreated: 2025-07-02T10:00:00.000Z
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

今回Javaのテスティングフレームワークには [JUnit 5](https://junit.org/junit5/)を利用します。JUnit 5の詳しい使い方に関しては公式ドキュメントを参照してください。

まず、Mavenプロジェクトを作成し、JUnit 5の依存関係を追加します。`pom.xml`ファイルを以下のように作成します。

```xml
<?xml version="1.0" encoding="UTF-8"?>
<project xmlns="http://maven.apache.org/POM/4.0.0"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd">
    <modelVersion>4.0.0</modelVersion>

    <groupId>com.example</groupId>
    <artifactId>java-tdd-fizzbuzz</artifactId>
    <version>1.0-SNAPSHOT</version>

    <properties>
        <maven.compiler.source>11</maven.compiler.source>
        <maven.compiler.target>11</maven.compiler.target>
        <project.build.sourceEncoding>UTF-8</project.build.sourceEncoding>
    </properties>

    <dependencies>
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter-api</artifactId>
            <version>5.10.2</version>
            <scope>test</scope>
        </dependency>
        <dependency>
            <groupId>org.junit.jupiter</groupId>
            <artifactId>junit-jupiter-engine</artifactId>
            <version>5.10.2</version>
            <scope>test</scope>
        </dependency>
    </dependencies>

    <build>
        <plugins>
            <plugin>
                <groupId>org.apache.maven.plugins</groupId>
                <artifactId>maven-surefire-plugin</artifactId>
                <version>3.2.5</version>
            </plugin>
        </plugins>
    </build>
</project>
```

続いて、簡単なテストケースを作成して、環境が正しく設定されていることを確認してみましょう。

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class HelloTest {
    @Test
    void test_greeting() {
        assertEquals("hello world", greeting());
    }
    
    private String greeting() {
        return "hello world";
    }
}
```

このテストファイルを `src/test/java/HelloTest.java` として保存し、テストを実行します。

```bash
$ mvn test
```

テストが成功すれば、開発環境が正しく設定されたことを確認できます。次に、テストが失敗するケースも試してみましょう。`hello world`を`hello world!!!`に変更してみます。

```java
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class HelloTest {
    @Test
    void test_greeting() {
        assertEquals("hello world!!!", greeting());
    }
    
    private String greeting() {
        return "hello world";
    }
}
```

このテストを実行すると、期待される値と実際の値が異なるため、テストは失敗します。

```bash
$ mvn test
```

```
[ERROR] Failures: 
[ERROR]   HelloTest.test_greeting:8 expected: <hello world!!!> but was: <hello world>
```

環境の設定が完了したので、テストファイルを元に戻しておきましょう。

続いてバージョン管理システムのセットアップをしておきましょう。バージョン管理システム何それ？だって！？君はセーブしないでロールプレイングゲームをクリアできるのか？できないならまず [ここ](https://backlog.com/ja/git-tutorial/intro/01/)でGitを使ったバージョン管理の基本を学んでおきましょう。

```bash
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

**1を渡したら文字列"1"を返す** プログラムを書きましょう。最初に何を書くのかって？
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

テストコードを書きます。日本語でテストケースを書くの？ですかって。開発体制にもよりますが日本人が開発するのであれば無理に英語で書くよりドキュメントとしての可読性が上がるのでテストコードであれば問題は無いと思います。

> テストコードを読みやすくするのは、テスト以外のコードを読みやすくするのと同じくらい大切なことだ。
> 
> —  リーダブルコード 

```java
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FizzBuzzTest {
    
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
}
```

このテストファイルを`src/test/java/FizzBuzzTest.java`として保存し、テストを実行します。

```bash
$ mvn test
```

```
[ERROR] Errors: 
[ERROR]   FizzBuzzTest.setup:11 » NoClassDefFound FizzBuzz
```

`FizzBuzz`クラスが定義されていないというエラーが出ました。そうですね、まだ作っていないのだから当然です。では`FizzBuzz::generate`メソッドを作りましょう。どんな振る舞いを書けばいいのでしょうか？とりあえず最初のテストを通すために **仮実装** から始めるとしましょう。

> 仮実装を経て本実装へ
> 
> 失敗するテストを書いてから、最初に行う実装はどのようなものだろうか----ベタ書きの値を返そう。
> 
> —  テスト駆動開発 

`FizzBuzz`クラスを定義して文字列リテラルを返す`generate`メソッドを作成しましょう。

```java
public class FizzBuzz {
    
    public String generate(int number) {
        return "1";
    }
}
```

このファイルを`src/main/java/FizzBuzz.java`として保存し、テストを実行します。

```bash
$ mvn test
```

```
[INFO] Tests run: 1, Failures: 0, Errors: 0, Skipped: 0
```

テストが通りました！これでTODOリストの最初の項目を片付けることができました。え？こんなベタ書きのプログラムでいいの？他に考えないといけないことたくさんあるんじゃない？ばかじゃないの？と思われるかもしませんが、この細かいステップに今しばらくお付き合いいただきたい。

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

```java
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FizzBuzzTest {
    
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
    
    @Test
    void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[ERROR] Failures: 
[ERROR]   FizzBuzzTest.test_2を渡したら文字列2を返す:19 expected: <2> but was: <1>
```

テストが失敗しました。それは文字列"1"しか返さないプログラムなのだから当然です。では1が渡されたら文字列"1"を返し、2を渡したら文字列"2"を返すようにプログラムを修正しましょう。数値を文字列に変換する必要があります。Javaでは`String.valueOf()`メソッドが使えます。

```java
public class FizzBuzz {
    
    public String generate(int number) {
        return String.valueOf(number);
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[INFO] Tests run: 2, Failures: 0, Errors: 0, Skipped: 0
```

テストが無事通りました。このように２つ目のテストによって`generate`メソッドの一般化を実現することができました。このようなアプローチを **三角測量** と言います。

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

### 明白な実装

次は「3 の倍数のときは数の代わりに｢Fizz｣と返す」というTODOに取り掛かりましょう。まず、3の倍数を渡すと"Fizz"を返すテストを書きます。

```java
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FizzBuzzTest {
    
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
    
    @Test
    void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
    }
    
    @Test
    void test_3を渡したら文字列Fizzを返す() {
        assertEquals("Fizz", fizzbuzz.generate(3));
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[ERROR] Failures: 
[ERROR]   FizzBuzzTest.test_3を渡したら文字列Fizzを返す:24 expected: <Fizz> but was: <3>
```

期待通りテストは失敗しました。3の倍数の場合に"Fizz"を返すように実装を修正しましょう。

```java
public class FizzBuzz {
    
    public String generate(int number) {
        if (number % 3 == 0) {
            return "Fizz";
        }
        return String.valueOf(number);
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[INFO] Tests run: 3, Failures: 0, Errors: 0, Skipped: 0
```

無事にテストが通りました。3の倍数の場合に"Fizz"を返すという要件を満たすことができました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - **3 の倍数のときは数の代わりに｢Fizz｣と返す**

  - 5 の倍数のときは｢Buzz｣と返す

  - 3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す

  - 1 から 100 までの数

  - プリントする

次は「5 の倍数のときは数の代わりに｢Buzz｣と返す」というTODOに取り掛かりましょう。まず、5の倍数を渡すと"Buzz"を返すテストを書きます。

```java
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FizzBuzzTest {
    
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
    
    @Test
    void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
    }
    
    @Test
    void test_3を渡したら文字列Fizzを返す() {
        assertEquals("Fizz", fizzbuzz.generate(3));
    }
    
    @Test
    void test_5を渡したら文字列Buzzを返す() {
        assertEquals("Buzz", fizzbuzz.generate(5));
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[ERROR] Failures: 
[ERROR]   FizzBuzzTest.test_5を渡したら文字列Buzzを返す:29 expected: <Buzz> but was: <5>
```

期待通りテストは失敗しました。5の倍数の場合に"Buzz"を返すように実装を修正しましょう。

```java
public class FizzBuzz {
    
    public String generate(int number) {
        if (number % 3 == 0) {
            return "Fizz";
        } else if (number % 5 == 0) {
            return "Buzz";
        }
        return String.valueOf(number);
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[INFO] Tests run: 4, Failures: 0, Errors: 0, Skipped: 0
```

無事にテストが通りました。5の倍数の場合に"Buzz"を返すという要件も満たすことができました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3 の倍数のときは数の代わりに｢Fizz｣と返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~

  - **3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す**

  - 1 から 100 までの数

  - プリントする

次は「3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す」というTODOに取り掛かりましょう。まず、15を渡すと"FizzBuzz"を返すテストを書きます。

```java
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;

class FizzBuzzTest {
    
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
    
    @Test
    void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
    }
    
    @Test
    void test_3を渡したら文字列Fizzを返す() {
        assertEquals("Fizz", fizzbuzz.generate(3));
    }
    
    @Test
    void test_5を渡したら文字列Buzzを返す() {
        assertEquals("Buzz", fizzbuzz.generate(5));
    }
    
    @Test
    void test_15を渡したら文字列FizzBuzzを返す() {
        assertEquals("FizzBuzz", fizzbuzz.generate(15));
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[ERROR] Failures: 
[ERROR]   FizzBuzzTest.test_15を渡したら文字列FizzBuzzを返す:34 expected: <FizzBuzz> but was: <Fizz>
```

期待通りテストは失敗しました。現在の実装では3の倍数の条件が先に評価されるため、15を渡すと"Fizz"が返ってきています。3と5の両方の倍数の場合に"FizzBuzz"を返すように実装を修正しましょう。

```java
public class FizzBuzz {
    
    public String generate(int number) {
        if (number % 3 == 0 && number % 5 == 0) {
            return "FizzBuzz";
        } else if (number % 3 == 0) {
            return "Fizz";
        } else if (number % 5 == 0) {
            return "Buzz";
        }
        return String.valueOf(number);
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[INFO] Tests run: 5, Failures: 0, Errors: 0, Skipped: 0
```

無事にテストが通りました。3と5の両方の倍数の場合に"FizzBuzz"を返すという要件も満たすことができました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3 の倍数のときは数の代わりに｢Fizz｣と返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~

  - ~~3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す~~

  - **1 から 100 までの数**

  - **プリントする**

最後に「1 から 100 までの数」と「プリントする」というTODOに取り掛かりましょう。まず、1から100までのFizzBuzzのリストを生成するメソッドを実装します。

```java
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import java.util.List;

class FizzBuzzTest {
    
    private FizzBuzz fizzbuzz;
    
    @BeforeEach
    void setup() {
        fizzbuzz = new FizzBuzz();
    }
    
    @Test
    void test_1を渡したら文字列1を返す() {
        assertEquals("1", fizzbuzz.generate(1));
    }
    
    @Test
    void test_2を渡したら文字列2を返す() {
        assertEquals("2", fizzbuzz.generate(2));
    }
    
    @Test
    void test_3を渡したら文字列Fizzを返す() {
        assertEquals("Fizz", fizzbuzz.generate(3));
    }
    
    @Test
    void test_5を渡したら文字列Buzzを返す() {
        assertEquals("Buzz", fizzbuzz.generate(5));
    }
    
    @Test
    void test_15を渡したら文字列FizzBuzzを返す() {
        assertEquals("FizzBuzz", fizzbuzz.generate(15));
    }
    
    @Test
    void test_1から100までのFizzBuzzを生成する() {
        List<String> result = fizzbuzz.generateList(100);
        
        assertEquals(100, result.size());
        assertEquals("1", result.get(0));
        assertEquals("2", result.get(1));
        assertEquals("Fizz", result.get(2));
        assertEquals("4", result.get(3));
        assertEquals("Buzz", result.get(4));
        assertEquals("Fizz", result.get(5));
        assertEquals("FizzBuzz", result.get(14));
        assertEquals("FizzBuzz", result.get(29));
        assertEquals("FizzBuzz", result.get(59));
        assertEquals("FizzBuzz", result.get(89));
        assertEquals("Buzz", result.get(99));
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[ERROR] Errors: 
[ERROR]   FizzBuzzTest.test_1から100までのFizzBuzzを生成する:41 » NoSuchMethod Method generateList(int) not found in class FizzBuzz
```

期待通りエラーになりました。`generateList`メソッドを実装しましょう。

```java
import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
    
    public String generate(int number) {
        if (number % 3 == 0 && number % 5 == 0) {
            return "FizzBuzz";
        } else if (number % 3 == 0) {
            return "Fizz";
        } else if (number % 5 == 0) {
            return "Buzz";
        }
        return String.valueOf(number);
    }
    
    public List<String> generateList(int count) {
        List<String> result = new ArrayList<>();
        for (int i = 1; i <= count; i++) {
            result.add(generate(i));
        }
        return result;
    }
}
```

テストを実行します。

```bash
$ mvn test
```

```
[INFO] Tests run: 6, Failures: 0, Errors: 0, Skipped: 0
```

無事にテストが通りました。1から100までのFizzBuzzのリストを生成するという要件も満たすことができました。

最後に、結果をコンソールに出力するメソッドを追加しましょう。

```java
import java.util.ArrayList;
import java.util.List;

public class FizzBuzz {
    
    public String generate(int number) {
        if (number % 3 == 0 && number % 5 == 0) {
            return "FizzBuzz";
        } else if (number % 3 == 0) {
            return "Fizz";
        } else if (number % 5 == 0) {
            return "Buzz";
        }
        return String.valueOf(number);
    }
    
    public List<String> generateList(int count) {
        List<String> result = new ArrayList<>();
        for (int i = 1; i <= count; i++) {
            result.add(generate(i));
        }
        return result;
    }
    
    public void printFizzBuzz(int count) {
        List<String> result = generateList(count);
        for (String s : result) {
            System.out.println(s);
        }
    }
}
```

これで実際に実行するためのMainクラスを作成しましょう。

```java
public class Main {
    public static void main(String[] args) {
        FizzBuzz fizzBuzz = new FizzBuzz();
        fizzBuzz.printFizzBuzz(100);
    }
}
```

Mainクラスを実行してFizzBuzzの出力を確認します。

```bash
$ mvn compile exec:java -Dexec.mainClass="Main"
```

これで「1 から 100 までの数」と「プリントする」というTODOも完了しました。

TODOリスト

  - ~~数を文字列にして返す~~
    
      - ~~1を渡したら文字列"1"を返す~~
    
      - ~~2を渡したら文字列"2"を返す~~

  - ~~3 の倍数のときは数の代わりに｢Fizz｣と返す~~

  - ~~5 の倍数のときは｢Buzz｣と返す~~

  - ~~3 と 5 両方の倍数の場合には｢FizzBuzz｣と返す~~

  - ~~1 から 100 までの数~~

  - ~~プリントする~~

おめでとうございます！すべてのTODOを完了し、FizzBuzzプログラムを実装することができました。

## まとめ

今回は **テスト駆動開発** の基本的なアプローチに従ってFizzBuzzプログラムを作成しました。TDDのサイクルである **赤（テストを失敗させる）→緑（テストを成功させる）→リファクタリング** を繰り返しながら、少しずつ機能を追加していくことで、着実にプログラムを完成させることができました。

この過程で、テスト駆動開発のいくつかの重要な概念を学びました：

1. **テストファースト**: テスト対象のコードを書く前にテストを書く
2. **アサートファースト**: テストの最後に期待する結果を最初に書く
3. **仮実装**: 失敗するテストを通すために、最初はベタ書きの値を返す
4. **三角測量**: 2つ以上の例があるときだけ一般化を行う
5. **明白な実装**: ロジックが明確な場合は直接的に実装する

テスト駆動開発を実践することで、コードの品質を確保しながら開発を進めることができます。また、テストが先にあることで、必要最小限の実装に集中でき、過剰な実装を避けることができます。

## 参考文献

- Kent Beck著 『テスト駆動開発』
- Martin Fowler著 『リファクタリング: 既存のコードを安全に改善する』
- Robert C. Martin著 『Clean Code: アジャイルソフトウェア達人の技』
