---
title: テスト駆動開発から始めるJava入門 ~ソフトウェア開発の三種の神器を準備する~
description: 
published: true
date: 2025-07-03T00:00:00.000Z
tags: 
editor: markdown
dateCreated: 2025-07-03T00:00:00.000Z
---

# エピソード2

## 初めに

この記事は [テスト駆動開発から始めるJava入門 ~2時間でTDDとリファクタリングのエッセンスを体験する~](テスト駆動開発から始めるJava入門1.md) の続編です。

## 自動化から始めるテスト駆動開発

エピソード1ではテスト駆動開発のゴールが **動作するきれいなコード** であることを学びました。では、良いコードを書き続けるためには何が必要になるでしょうか？それは[ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)と呼ばれるものです。

> 今日のソフトウェア開発の世界において絶対になければならない3つの技術的な柱があります。
> 三本柱と言ったり、三種の神器と言ったりしていますが、それらは
> 
>   - バージョン管理
> 
>   - テスティング
> 
>   - 自動化
> 
> の3つです。
> 
> —  https://t-wada.hatenablog.jp/entry/clean-code-that-works 

**バージョン管理** と **テスティング** に関してはエピソード1で触れました。本エピソードでは最後の **自動化** に関しての解説と次のエピソードに備えたセットアップ作業を実施しておきたいと思います。ですがその前に **バージョン管理** で1つだけ解説しておきたいことがありますのでそちらから進めて行きたいと思います。

### コミットメッセージ

これまで作業の区切りにごとにレポジトリにコミットしていましたがその際に以下のような書式でメッセージを書いていました。

``` bash
$ git commit -m 'refactor: メソッドの抽出'
```

この書式は [Angularルール](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#type) に従っています。具体的には、それぞれのコミットメッセージはヘッダ、ボディ、フッタで構成されています。ヘッダはタイプ、スコープ、タイトルというフォーマットで構成されています。

    <タイプ>(<スコープ>): <タイトル>
    <空行>
    <ボディ>
    <空行>
    <フッタ>

ヘッダは必須です。 ヘッダのスコープは任意です。 コミットメッセージの長さは50文字までにしてください。

(そうすることでその他のGitツールと同様にGitHub上で読みやすくなります。)

コミットのタイプは次を用いて下さい。

  - feat: A new feature (新しい機能)

  - fix: A bug fix (バグ修正)

  - docs: Documentation only changes (ドキュメント変更のみ)

  - style: Changes that do not affect the meaning of the code
    (white-space, formatting, missing semi-colons, etc) (コードに影響を与えない変更)

  - refactor: A code change that neither fixes a bug nor adds a feature
    (機能追加でもバグ修正でもないコード変更)

  - perf: A code change that improves performance (パフォーマンスを改善するコード変更)

  - test: Adding missing or correcting existing tests
    (存在しないテストの追加、または既存のテストの修正)

  - chore: Changes to the build process or auxiliary tools and libraries
    such as documentation generation
    (ドキュメント生成のような、補助ツールやライブラリやビルドプロセスの変更)

コミットメッセージにつけるプリフィックスに関しては [【今日からできる】コミットメッセージに 「プレフィックス」をつけるだけで、開発効率が上がった話](https://qiita.com/numanomanu/items/45dd285b286a1f7280ed)を参照ください。

### 依存関係管理

では **自動化** の準備に入りたいのですがそのためにはいくつかの外部プログラムを利用する必要があります。そのためのツールが **Gradle** です。

> Gradleとは、Javaで記述されたビルドツール・依存関係管理ツールです。Gradleで扱う依存関係をdependenciesとして管理し、プロジェクトのビルドプロセスを自動化できます。
> 
> —  Gradle Documentation 

**Gradle** はすでに何度か使っています。例えばエピソード1の初めの `JUnit 5` や `AssertJ` のインストールなどです。

``` bash
$ ./gradlew build
```

では、これからもこのようにして必要な外部プログラムを一つ一つ追加していくのでしょうか？また、開発用マシンを変えた時にも同じことを繰り返さないといけないのでしょうか？面倒ですよね。そのような面倒なことをしないで済む仕組みがJavaには用意されています。それが **Gradle依存関係管理** です。

> Gradle依存関係管理とは、作成したアプリケーションがどのライブラリに依存しているか、そしてインストールしているバージョンはいくつかという情報を管理するための仕組みです。
> 
> —  Gradle Documentation 

**Gradle** を使って依存関係を管理しましょう。

``` bash
$ ./gradlew init --type java-application
```

`build.gradle` が作成されます。

``` groovy
plugins {
    id 'java'
    id 'application'
}

repositories {
    mavenCentral()
}

dependencies {
    testImplementation 'org.junit.jupiter:junit-jupiter-api:5.10.1'
    testRuntimeOnly 'org.junit.jupiter:junit-jupiter-engine:5.10.1'
    implementation 'com.google.guava:guava:31.1-jre'
}

application {
    mainClass = 'App'
}

test {
    useJUnitPlatform()
}
```

dependencies部分を以下の様に書き換えます。

``` groovy
plugins {
    id 'java'
    id 'application'
    id 'jacoco'
    id 'checkstyle'
    id 'pmd'
    id 'com.github.spotbugs' version '6.0.7'
}

repositories {
    mavenCentral()
}

dependencies {
    // テスト関連
    testImplementation 'org.junit.jupiter:junit-jupiter-api:5.10.1'
    testImplementation 'org.junit.jupiter:junit-jupiter-params:5.10.1'
    testRuntimeOnly 'org.junit.jupiter:junit-jupiter-engine:5.10.1'
    testImplementation 'org.assertj:assertj-core:3.24.2'
    
    // SpotBugs関連
    spotbugsPlugins 'com.h3xstream.findsecbugs:findsecbugs-plugin:1.12.0'
}

application {
    mainClass = 'FizzBuzz'
}
```

書き換えたら `./gradlew build` で依存関係をインストールします。

``` bash
$ ./gradlew build
Starting a Gradle Daemon (subsequent builds will be faster)

BUILD SUCCESSFUL in 8s
7 actionable tasks: 7 executed
```

これで次の準備ができました。

### 静的コード解析

良いコードを書き続けるためにはコードの品質を維持していく必要があります。エピソード1では **テスト駆動開発** によりプログラムを動かしながら品質の改善していきました。出来上がったコードに対する品質チェックの方法として **静的コード解析** があります。Java用 **静的コード解析** ツール[Checkstyle](https://checkstyle.sourceforge.io/)、[PMD](https://pmd.github.io/)、[SpotBugs](https://spotbugs.github.io/) を使って確認してみましょう。プログラムは先程 **Gradle** を使ってインストールしたので以下のコマンドを実行します。

``` bash
$ ./gradlew checkstyleMain
```

何かいろいろ出てきましたね。Checkstyleの詳細に関しては [Checkstyle Documentation](https://checkstyle.sourceforge.io/) を参照ください。PMDでも試してみましょう。

``` bash
$ ./gradlew pmdMain
```

また何やら出てきましたね。[PMDルール](https://pmd.github.io/latest/pmd_rules_java.html)のメッセージを調べたところ、`FizzBuzz.java` の以下のコードは書き方がよろしくないようですね。

``` java
public class FizzBuzz {
    public static void main(String[] args) {
        FizzBuzz fizzBuzz = new FizzBuzz();
        // Magic Number の使用
        int max = 100;
        // ...
    }
}
```

**定数の抽出** を使ってコードをリファクタリングしておきましょう。

``` java
public class FizzBuzz {
    private static final int DEFAULT_MAX = 100;
    
    public static void main(String[] args) {
        FizzBuzz fizzBuzz = new FizzBuzz();
        int max = DEFAULT_MAX;
        // ...
    }
}
```

再度確認します。チェックは通りましたね。

``` bash
$ ./gradlew checkstyleMain pmdMain
```

テストも実行して壊れていないかも確認しておきます。

``` bash
$ ./gradlew test

> Task :test

FizzBuzz テスト > 数字の1は文字列の1を返す PASSED
FizzBuzz テスト > 数字の2は文字列の2を返す PASSED
FizzBuzz テスト > 3で割り切れる数字はFizzを返す PASSED
// ... 61個のテストすべてPASSED

BUILD SUCCESSFUL in 6s
3 actionable tasks: 1 executed, 2 up-to-date
```

品質管理ツールの設定ファイルを作成しましょう。`config/checkstyle/checkstyle.xml` を作成します。

``` bash
$ mkdir -p config/checkstyle
```

``` xml
<?xml version="1.0"?>
<!DOCTYPE module PUBLIC
        "-//Checkstyle//DTD Checkstyle Configuration 1.3//EN"
        "https://checkstyle.org/dtds/configuration_1_3.dtd">

<module name="Checker">
    <property name="charset" value="UTF-8"/>
    
    <!-- ファイルレベルのチェック -->
    <module name="FileTabCharacter">
        <property name="eachLine" value="true"/>
    </module>
    
    <!-- TreeWalkerによるAST解析 -->
    <module name="TreeWalker">
        <!-- インデント -->
        <module name="Indentation">
            <property name="basicOffset" value="4"/>
        </module>
        
        <!-- 命名規則 -->
        <module name="TypeName"/>
        <module name="MethodName"/>
        <module name="VariableName"/>
        <module name="ConstantName"/>
        
        <!-- その他 -->
        <module name="EmptyStatement"/>
        <module name="EqualsHashCode"/>
        <module name="MagicNumber">
            <property name="ignoreHashCodeMethod" value="true"/>
            <property name="ignoreAnnotation" value="true"/>
        </module>
    </module>
</module>
```

セットアップができたのでここでコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'chore: 静的コード解析セットアップ'
```

### コードフォーマッタ

良いコードであるためにはフォーマットも大切な要素です。

> 優れたソースコードは「目に優しい」ものでなければいけない。
> 
> —  リーダブルコード 

Javaにはいくつかフォーマットアプリケーションはあるのですがここは `Checkstyle` の機能を使って実現することにしましょう。以下のコードのフォーマットをわざと崩してみます。

``` java
public class FizzBuzz {
    private static final int DEFAULT_MAX = 100;

    public String convert(int number) {
            boolean isFizz = number % 3 == 0;
        boolean isBuzz = number % 5 == 0;

        if (isFizz && isBuzz) {
        return "FizzBuzz";
        }
        if (isFizz) {
        return "Fizz";
        }
        if (isBuzz) {
        return "Buzz";
        }

        return String.valueOf(number);
    }
}
```

Checkstyleでチェックしてみます。

``` bash
$ ./gradlew checkstyleMain

> Task :checkstyleMain FAILED

FAILURE: Build failed with an exception.

* What went wrong:
Execution failed for task ':checkstyleMain'.
> Checkstyle rule violations were found. See the report at: file:///path/to/build/reports/checkstyle/main.html
  [ant:checkstyle] [ERROR] /path/to/FizzBuzz.java:7:5: Indentation 'method def' has incorrect indentation level 4, expected level should be 8.
```

編集した部分が `Indentation 'method def' has incorrect indentation level 4, expected level should be 8.` と指摘されています。手動で修正しておきましょう。

``` java
public class FizzBuzz {
    private static final int DEFAULT_MAX = 100;

    public String convert(int number) {
        boolean isFizz = number % 3 == 0;
        boolean isBuzz = number % 5 == 0;

        if (isFizz && isBuzz) {
            return "FizzBuzz";
        }
        if (isFizz) {
            return "Fizz";
        }
        if (isBuzz) {
            return "Buzz";
        }

        return String.valueOf(number);
    }
}
```

``` bash
$ ./gradlew checkstyleMain

BUILD SUCCESSFUL in 2s
1 actionable task: 1 executed
```

フォーマットが修正されたことが確認できましたね。

### コードカバレッジ

静的コード解析による品質の確認はできました。では動的なテストに関してはどうでしょうか？ **コードカバレッジ** を確認する必要があります。

> コード網羅率（コードもうらりつ、英: Code coverage）コードカバレッジは、ソフトウェアテストで用いられる尺度の1つである。プログラムのソースコードがテストされた割合を意味する。この場合のテストはコードを見ながら行うもので、ホワイトボックステストに分類される。
> 
> —  ウィキペディア 

Java用 **コードカバレッジ** 検出プログラムとして [JaCoCo](https://www.jacoco.org/jacoco/)を使います。build.gradleに追加して **Gradle** でインストールをしましょう。

``` groovy
plugins {
    id 'java'
    id 'application'
    id 'jacoco'
    // ... 他のプラグイン
}

// ... 他の設定

// JaCoCo（コードカバレッジ）設定
jacoco {
    toolVersion = "0.8.11"
}

jacocoTestReport {
    dependsOn test
    reports {
        xml.required = false
        csv.required = false
        html.outputLocation = layout.buildDirectory.dir('jacocoHtml')
    }
}
```

``` bash
$ ./gradlew build
```

テストを実施します。

``` bash
$ ./gradlew test

> Task :test

FizzBuzz テスト > 数字の1は文字列の1を返す PASSED
FizzBuzz テスト > 数字の2は文字列の2を返す PASSED
FizzBuzz テスト > 3で割り切れる数字はFizzを返す PASSED
// ... 61個のテストすべてPASSED

BUILD SUCCESSFUL in 6s
3 actionable tasks: 1 executed, 2 up-to-date
```

コードカバレッジレポートを生成します。

``` bash
$ ./gradlew jacocoTestReport

BUILD SUCCESSFUL in 2s
4 actionable tasks: 1 executed, 3 up-to-date
```

テスト実行後に `build/jacocoHtml` というフォルダが作成されます。その中の `index.html` を開くとカバレッジ状況を確認できます。セットアップが完了したらコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'chore: コードカバレッジセットアップ'
```

### タスクランナー

ここまででテストの実行、静的コード解析、コードフォーマット、コードカバレッジを実施することができるようになりました。でもコマンドを実行するのにそれぞれコマンドを覚えておくのは面倒ですよね。例えばテストの実行は

``` bash
$ ./gradlew test

> Task :test

FizzBuzz テスト > 数字の1は文字列の1を返す PASSED
// ... 61個のテストすべてPASSED

BUILD SUCCESSFUL in 6s
3 actionable tasks: 1 executed, 2 up-to-date
```

このようにしていました。では静的コードの解析はどうやりましたか？フォーマットはどうやりましたか？調べるのも面倒ですよね。いちいち調べるのが面倒なことは全部 **タスクランナー** にやらせるようにしましょう。

> タスクランナーとは、アプリケーションのビルドなど、一定の手順で行う作業をコマンド一つで実行できるように予めタスクとして定義したものです。
> 
> —  Gradle Documentation 

Javaの **タスクランナー** は `Gradle` です。

> GradleはJavaにおけるタスクランナーです。gradlewコマンドと起点となるbuild.gradleというタスクを記述するファイルを用意することで、タスクの実行や登録されたタスクの一覧表示を行えます。
> 
> —  Gradle Documentation 

早速、テストタスクから作成しましょう。`build.gradle` にカスタムタスクを追加します。

``` groovy
// カスタムタスク：TDD用の継続的テスト実行
task tdd(type: Test) {
    useJUnitPlatform()
    testLogging {
        events "passed", "skipped", "failed"
        exceptionFormat "full"
    }
    outputs.upToDateWhen { false }
}

// カスタムタスク：品質チェック全実行
task qualityCheck {
    dependsOn 'checkstyleMain', 'checkstyleTest', 'pmdMain', 'pmdTest', 'spotbugsMain', 'spotbugsTest'
    description 'Run all quality checks'
    group 'verification'
}

// カスタムタスク：すべてのチェックとテストを実行
task fullCheck {
    dependsOn 'test', 'qualityCheck', 'jacocoTestReport'
    description 'Run all tests and quality checks'
    group 'verification'
}
```

タスクが登録されたか確認してみましょう。

``` bash
$ ./gradlew tasks --group verification

Verification tasks
------------------
check - Runs all checks.
checkstyleMain - Run Checkstyle analysis for main classes.
checkstyleTest - Run Checkstyle analysis for test classes.
fullCheck - Run all tests and quality checks
pmdMain - Run PMD analysis for main classes.
pmdTest - Run PMD analysis for test classes.
qualityCheck - Run all quality checks
spotbugsMain - Run SpotBugs analysis for main classes.
spotbugsTest - Run SpotBugs analysis for test classes.
test - Runs the unit tests.
tdd - Runs the unit tests.
```

タスクが登録されたことが確認できたのでタスクを実行します。

``` bash
$ ./gradlew tdd

> Task :tdd

FizzBuzz テスト > 数字の1は文字列の1を返す PASSED
FizzBuzz テスト > 数字の2は文字列の2を返す PASSED
FizzBuzz テスト > 3で割り切れる数字はFizzを返す PASSED
// ... 61個のテストすべてPASSED

BUILD SUCCESSFUL in 6s
3 actionable tasks: 1 executed, 2 up-to-date
```

テストタスクが実行されたことが確認できたので引き続き静的コードの解析タスクを追加します。

``` bash
$ ./gradlew qualityCheck

> Task :checkstyleMain
> Task :checkstyleTest
> Task :pmdMain
> Task :pmdTest
> Task :spotbugsMain
> Task :spotbugsTest
> Task :qualityCheck

BUILD SUCCESSFUL in 4s
6 actionable tasks: 6 executed
```

全品質チェックタスクが実行されたことが確認できました。セットアップができたのでコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'chore: タスクランナーセットアップ'
```

### タスクの自動化

良いコードを書くためのタスクをまとめることができました。でも、どうせなら自動で実行できるようにしたいですよね。Javaの世界では **Gradle Continuous Build** や **Gradle File Watching** の機能を使ってタスクを自動実行することができます。

``` bash
$ ./gradlew test --continuous
```

このコマンドを実行すると、ファイルの変更を監視して自動的にテストを実行してくれるようになります。また、IntelliJ IDEAやVS Code等のIDEの **Auto Test** 機能を使うことで、ファイル変更時に自動でテストが実行されるように設定することも可能です。

より高度な自動化のために、Gradle Watch Service プラグインを使うこともできます。

``` groovy
plugins {
    // 他のプラグイン
    id 'org.gradle.test-retry' version '1.5.6'
}

// 自動実行設定
test {
    useJUnitPlatform()
    
    // テスト失敗時のリトライ設定
    retry {
        maxRetries = 3
        maxFailures = 20
        failOnPassedAfterRetry = false
    }
    
    // テストログ設定
    testLogging {
        events "passed", "skipped", "failed"
        exceptionFormat "full"
    }
}

// ファイル変更監視タスク
task watchTest {
    doLast {
        println "Watching for file changes..."
        println "Run: ./gradlew test --continuous"
        println "Or use your IDE's auto-test feature"
    }
}
```

起動したらソースファイルを編集してテストが自動実行されるか確認しましょう。

``` java
public class FizzBuzz {
    // わざとテストを失敗させる変更
    public String convert(int number) {
        if (number <= 0) {
            throw new IllegalArgumentException("Input must be positive number");
        }
        
        if (number % 15 == 0) {
            return "FizzBuzzBuzz"; // わざと間違えてみる
        }
        
        if (number % 3 == 0) {
            return "Fizz";
        }
        
        if (number % 5 == 0) {
            return "Buzz";
        }
        
        return String.valueOf(number);
    }
}
```

変更を感知してテストが実行された結果、失敗することが確認できます。コードを元に戻してテストをパスするようにしておきましょう。

``` java
public class FizzBuzz {
    public String convert(int number) {
        if (number <= 0) {
            throw new IllegalArgumentException("Input must be positive number");
        }
        
        if (number % 15 == 0) {
            return "FizzBuzz"; // 修正
        }
        
        if (number % 3 == 0) {
            return "Fizz";
        }
        
        if (number % 5 == 0) {
            return "Buzz";
        }
        
        return String.valueOf(number);
    }
}
```

テストがパスすることが確認できたらコミットしておきましょう。

``` bash
$ git add .
$ git commit -m 'chore: タスクの自動化'
```

これで [ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works) の最後のアイテムの準備ができました。次回の開発からは最初にコマンドラインで `./gradlew test --continuous` を実行すれば良いコードを書くためのタスクを自動でやってくれるようになるのでコードを書くことに集中できるようになりました。では、次のエピソードに進むとしましょう。

## まとめ

本エピソードでは、Javaでソフトウェア開発の三種の神器を実践しました：

### 1. バージョン管理
- **Git**: プロジェクトの変更履歴管理
- **Angularコミットメッセージ規約**: 一貫性のあるコミットメッセージ

### 2. テスティング
- **JUnit 5**: モダンなJavaテストフレームワーク
- **AssertJ**: 流暢で可読性の高いアサーション
- **パラメータ化テスト**: 効率的な複数入力値テスト
- **学習テスト**: Java言語機能の理解促進

### 3. 自動化
- **Gradle**: ビルドツール・タスクランナー
- **JaCoCo**: コードカバレッジ測定
- **Checkstyle**: コーディング規約チェック
- **PMD**: 静的コード解析
- **SpotBugs**: バグパターン検出
- **継続的ビルド**: ファイル変更の自動監視

これらのツールと技法により、**動作するきれいなコード** を継続的に書き続けることができる環境が整いました。次のエピソードでは、より高度なテスト技法やリファクタリングのパターンを学習していきます。
