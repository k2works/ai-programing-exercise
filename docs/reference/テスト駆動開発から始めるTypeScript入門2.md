---
title: テスト駆動開発から始めるTypeScript入門 ~ソフトウェア開発の三種の神器を準備する~
description: TypeScriptでのソフトウェア開発三種の神器（バージョン管理、テスティング、自動化）の準備
published: true
date: 2025-08-06T02:26:05.112Z
tags: tdd, typescript, 自動化, テスト駆動開発
editor: markdown
dateCreated: 2025-07-03T06:14:52.352Z
---

# エピソード2

## 初めに

この記事は「テスト駆動開発から始めるTypeScript入門 ~2時間でTDDとリファクタリングのエッセンスを体験する~」の続編です。

## 自動化から始めるテスト駆動開発

エピソード1ではテスト駆動開発のゴールが **動作するきれいなコード** であることを学びました。では、良いコードを書き続けるためには何が必要になるでしょうか？それは[ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works)と呼ばれるものです。

> 今日のソフトウェア開発の世界において絶対になければならない3つの技術的な柱があります。
> 三本柱と言ったり、三種の神器と言ったりしていますが、それらは
> 
>   - バージョン管理
>   - テスティング
>   - 自動化
> 
> の3つです。
> 
> —  https://t-wada.hatenablog.jp/entry/clean-code-that-works 

**バージョン管理** と **テスティング** に関してはエピソード1で触れました。本エピソードでは最後の **自動化** に関しての解説と次のエピソードに備えたセットアップ作業を実施しておきたいと思います。ですがその前に **バージョン管理** で1つだけ解説しておきたいことがありますのでそちらから進めて行きたいと思います。

### コミットメッセージ

これまで作業の区切りにごとにレポジトリにコミットしていましたがその際に以下のような書式でメッセージを書いていました。

```bash
$ git commit -m 'refactor: メソッドの抽出'
```

この書式は [Conventional Commits](https://www.conventionalcommits.org/ja/)に従っています。具体的には、それぞれのコミットメッセージはヘッダ、ボディ、フッタで構成されています。ヘッダはタイプ、スコープ、タイトルというフォーマットで構成されています。

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

### パッケージマネージャ

では **自動化** の準備に入りたいのですがそのためにはいくつかの外部プログラムを利用する必要があります。そのためのツールが **npm** です。

> npmとは、Node.jsで記述されたサードパーティ製のライブラリを管理するためのツールで、npmで扱うライブラリをパッケージと呼びます。
> 
> —  Node.js公式ドキュメント 

**npm** はすでに何度か使っています。例えばエピソード1の初めの `vitest` のインストールなどです。

```bash
$ npm install -D vitest
```

では、これからもこのようにして必要な外部プログラムを一つ一つインストールしていくのでしょうか？また、開発用マシンを変えた時にも同じことを繰り返さないといけないのでしょうか？面倒ですよね。そのような面倒なことをしないで済む仕組みがNode.jsには用意されています。それが **package.json** です。

> package.jsonとは、作成したアプリケーションがどのパッケージに依存しているか、そしてインストールしているバージョンはいくつかという情報を管理するためのファイルです。
> 
> —  Node.js公式ドキュメント 

**npm init** を使ってパッケージを管理しましょう。

```bash
$ npm init -y
```

`package.json` が作成されます。

```json
{
  "name": "app",
  "version": "1.0.0",
  "description": "",
  "main": "index.js",
  "scripts": {
    "test": "echo \"Error: no test specified\" && exit 1"
  },
  "keywords": [],
  "author": "",
  "license": "ISC"
}
```

`"scripts"` の部分を以下の様に書き換えます。

```json
{
  "name": "app",
  "private": true,
  "version": "0.0.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "c8 vitest run",
    "lint": "eslint . --ext .ts,.tsx",
    "lint:fix": "eslint . --ext .ts,.tsx --fix",
    "format": "prettier --write .",
    "format:check": "prettier --check .",
    "gulp": "gulp",
    "watch": "gulp watch",
    "guard": "gulp guard",
    "check": "gulp checkAndFix",
    "commit": "git add . && git commit",
    "setup": "npm install && npm run check"
  }
}
```

書き換えたら `npm install` でパッケージをインストールします。

```bash
$ npm install
added 507 packages, and audited 508 packages in 12s

101 packages are looking for funding
  run `npm fund` for details

found 0 vulnerabilities
```

これで次の準備ができました。

### 静的コード解析

良いコードを書き続けるためにはコードの品質を維持していく必要があります。エピソード1では **テスト駆動開発** によりプログラムを動かしながら品質の改善していきました。出来上がったコードに対する品質チェックの方法として **静的コード解析** があります。TypeScript用 **静的コード解析** ツール[ESLint](https://eslint.org/) を使って確認してみましょう。プログラムは先程 **npm** を使ってインストールしたので以下のコマンドを実行します。

```bash
$ npm run lint
> app@0.0.0 lint
> eslint . --ext .ts,.tsx
```

なにかいろいろ出てきましたね。ESLintの詳細に関しては [ESLint ルールまとめ](https://qiita.com/mysticatea/items/f523dab04a25f617c87d)を参照ください。`--fix` オプションをつけて実施してみましょう。

```bash
$ npm run lint:fix
> app@0.0.0 lint:fix
> eslint . --ext .ts,.tsx --fix
```

また何やら出てきましたね。エラーメッセージを調べたところ、`array-learning.test.ts` の以下の学習用テストコードは書き方がよろしくないようですね。

```typescript
// ...
  it('selectメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => Number.isInteger(n))
    expect(result).toEqual([2, 4])
  })

  it('filterメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => Number.isInteger(n))
    expect(result).toEqual([2, 4])
  })
// ...
```

**説明用変数の導入** を使ってテストコードをリファクタリングしておきましょう。

```typescript
// ...
  it('selectメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => Number.isInteger(n))
    
    expect(result).toEqual([2, 4])
  })

  it('find_allメソッドで特定の条件を満たす要素だけを配列に入れて返す', () => {
    const numbers = [1.1, 2, 3.3, 4]
    const result = numbers.filter((n) => Number.isInteger(n))
    
    expect(result).toEqual([2, 4])
  })
// ...
```

再度確認します。チェックは通りましたね。

```bash
$ npm run lint
> app@0.0.0 lint
> eslint . --ext .ts,.tsx

$ # エラーなし
```

テストも実行して壊れていないかも確認しておきます。

```bash
$ npm run test
> app@0.0.0 test
> vitest run

 ✓ src/array-learning.test.ts (12 tests) 5ms
 ✓ src/fizz-buzz.test.ts (25 tests) 6ms

 Test Files  2 passed (2)
      Tests  37 passed (37)
   Start at  05:58:15
   Duration  5.44s (transform 142ms, setup 0ms, collect 252ms, tests 13ms, environment 7.88s, prepare 1.48s)
```

いちいち調べるのも手間なので自動で修正できるところは修正してもらいましょう。

```bash
$ npm run lint:fix
```

再度確認します。

```bash
$ npm run lint
> app@0.0.0 lint
> eslint . --ext .ts,.tsx

$ # エラーなし
```

まだ、自動修正できなかった部分があるかもしれませんが、この部分はチェック対象から外すことにしましょう。ESLintの設定ファイル `eslint.config.js` を確認して、必要に応じて除外設定を追加します。

```javascript
export default [
  // ...existing config...
  {
    files: ['**/*.test.{ts,tsx}'],
    rules: {
      '@typescript-eslint/no-unused-expressions': 'off',
      '@typescript-eslint/no-unused-vars': 'warn'
    }
  }
]
```

再度チェックを実行します。

```bash
$ npm run lint
> app@0.0.0 lint
> eslint . --ext .ts,.tsx

$ # エラーなし
```

循環的複雑度 (Cyclomatic complexity)は７で設定しておきます

> 循環的複雑度 (Cyclomatic complexity)
> 循環的複雑度(サイクロマティック複雑度)とは、ソフトウェア測定法の一つであり、コードがどれぐらい> 複雑であるかをメソッド単位で数値にして表す指標。

ESLintの設定に循環的複雑度の制限を追加しましょう。一般的に、循環的複雑度は7以下に保つことが推奨されています。

```javascript
// eslint.config.js
export default [
  // ...既存の設定...
  {
    files: ['**/*.ts', '**/*.tsx'],
    rules: {
      // ...既存のルール...
      // 循環的複雑度の制限 - 7を超える場合はエラー
      'complexity': ['error', { max: 7 }],
    },
  },
]
```

設定を追加したらESLintを実行して確認してみましょう。

```bash
$ npm run lint
```

循環的複雑度の制限により、以下の効果が得られます：

- **可読性向上**: 小さなメソッドは理解しやすい
- **保守性向上**: 変更の影響範囲が限定される
- **テスト容易性**: 個別機能のテストが簡単
- **自動品質管理**: 複雑なコードの混入を自動防止

このように、ESLintルールを活用することで、継続的にコード品質を保つことができます。

セットアップができたのでここでコミットしておきましょう。

```bash
$ git add .
$ git commit -m 'chore: 静的コード解析セットアップ'
```

### コードフォーマッタ

良いコードであるためにはフォーマットも大切な要素です。

> 優れたソースコードは「目に優しい」ものでなければいけない。
> 
> —  リーダブルコード 

TypeScriptにはいくつかフォーマットアプリケーションはあるのですがここは `Prettier` を使って実現することにしましょう。以下のコードのフォーマットをわざと崩してみます。

```typescript
export class FizzBuzz {
  private static readonly MAX_NUMBER = 100

  public static generate(number: number): string {
          const isFizz = number % 3 === 0
    const isBuzz = number % 5 === 0

    if (isFizz && isBuzz) return 'FizzBuzz'
    if (isFizz) return 'Fizz'
    if (isBuzz) return 'Buzz'

    return number.toString()
  }

  public static generateList(): string[] {
    // 1から最大値までのFizzBuzz配列を1発で作る
    return Array.from({ length: this.MAX_NUMBER }, (_, i) => this.generate(i + 1))
  }
}
```

フォーマットチェックをしてみます。

```bash
$ npm run format:check
> app@0.0.0 format:check
> prettier --check .

Checking formatting...
src/fizz-buzz.ts
[warn] Code style issues found in the above file. Run Prettier with --write to fix.
```

編集した部分が `Code style issues found` と指摘されています。`--write` オプションで自動修正しておきましょう。

```bash
$ npm run format
> app@0.0.0 format
> prettier --write .

src/fizz-buzz.ts 13ms
```

```typescript
export class FizzBuzz {
  private static readonly MAX_NUMBER = 100

  public static generate(number: number): string {
    const isFizz = number % 3 === 0
    const isBuzz = number % 5 === 0

    if (isFizz && isBuzz) return 'FizzBuzz'
    if (isFizz) return 'Fizz'
    if (isBuzz) return 'Buzz'

    return number.toString()
  }

  public static generateList(): string[] {
    // 1から最大値までのFizzBuzz配列を1発で作る
    return Array.from({ length: this.MAX_NUMBER }, (_, i) => this.generate(i + 1))
  }
}
```

```bash
$ npm run format:check
> app@0.0.0 format:check
> prettier --check .

Checking formatting...
All matched files use Prettier code style!
```

フォーマットが修正されたことが確認できましたね。ちなみに `npm run lint:fix` でもフォーマットをしてくれるので通常はこちらのオプションで問題ないと思います。

### コードカバレッジ

静的コードコード解析による品質の確認はできました。では動的なテストに関してはどうでしょうか？ **コードカバレッジ** を確認する必要あります。

> コード網羅率（コードもうらりつ、英: Code coverage）コードカバレッジは、ソフトウェアテストで用いられる尺度の1つである。プログラムのソースコードがテストされた割合を意味する。この場合のテストはコードを見ながら行うもので、ホワイトボックステストに分類される。
> 
> —  ウィキペディア 

TypeScript用 **コードカバレッジ** 検出プログラムとして [c8](https://github.com/bcoe/c8)を使います。package.jsonに追加して **npm** でインストールをしましょう。

```json
{
  "name": "app",
  "private": true,
  "version": "0.0.0",
  "type": "module",
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "vitest run --coverage",
    "lint": "eslint . --ext .ts,.tsx",
    "lint:fix": "eslint . --ext .ts,.tsx --fix",
    "format": "prettier --write .",
    "format:check": "prettier --check .",
    "gulp": "gulp",
    "watch": "gulp watch",
    "guard": "gulp guard",
    "check": "gulp checkAndFix"
  },
  "devDependencies": {
    "@typescript-eslint/eslint-plugin": "^8.35.1",
    "@typescript-eslint/parser": "^8.35.1",
    "c8": "^10.1.3",
    "eslint": "^9.30.1",
    "eslint-config-prettier": "^10.1.5",
    "eslint-plugin-prettier": "^5.5.1",
    "gulp": "^5.0.1",
    "gulp-shell": "^0.8.0",
    "prettier": "^3.6.2",
    "typescript": "~5.8.3",
    "vite": "^7.0.0",
    "vitest": "^3.2.4"
  }
}
```

```bash
$ npm install --save-dev vitest @vitest/coverage-v8
added 0 packages, and audited 507 packages in 2s

101 packages are looking for funding
  run `npm fund` for details

found 0 vulnerabilities
```

サイトの説明に従ってカバレッジ設定ファイルを作成します。

```json
{
  "reporter": ["text", "html", "json"],
  "reportsDirectory": "coverage",
  "exclude": [
    "dist/**",
    "node_modules/**",
    "**/*.test.ts",
    "**/*.config.js",
    "**/*.config.ts",
    "vite.config.ts",
    "vitest.config.ts",
    "gulpfile.js",
    "src/main.ts",
    "src/counter.ts",
    "src/vite-env.d.ts"
  ],
  "all": true,
  "checkCoverage": false
}
```

テストを実施します。

```bash
$ npm run test:coverage
> app@0.0.0 test:coverage
> c8 vitest run

 ✓ src/array-learning.test.ts (12 tests) 5ms
 ✓ src/fizz-buzz.test.ts (25 tests) 6ms

 Test Files  2 passed (2)
      Tests  37 passed (37)
   Start at  05:50:28
   Duration  5.49s (transform 162ms, setup 0ms, collect 289ms, tests 11ms, environment 7.81s, prepare 1.61s)

----------------------|---------|----------|---------|---------|-------------------
File                  | % Stmts | % Branch | % Funcs | % Lines | Uncovered Line #s 
----------------------|---------|----------|---------|---------|-------------------
All files             |       0 |        0 |       0 |       0 |                   
 src                  |       0 |        0 |       0 |       0 |                   
  fizz-buzz.ts        |       0 |        0 |       0 |       0 | 1-48              
----------------------|---------|----------|---------|---------|-------------------
```

テスト実行後に `coverage` というフォルダが作成されます。その中の `index.html` を開くとカバレッジ状況を確認できます。セットアップが完了したらコミットしておきましょう。

```bash
$ git add .
$ git commit -m 'chore: コードカバレッジセットアップ'
```

### タスクランナー

ここまででテストの実行、静的コード解析、コードフォーマット、コードカバレッジを実施することができるようになりました。でもコマンドを実行するのにそれぞれコマンドを覚えておくのは面倒ですよね。例えばテストの実行は

```bash
$ npm run test
> app@0.0.0 test
> vitest run

 ✓ src/array-learning.test.ts (12 tests) 5ms
 ✓ src/fizz-buzz.test.ts (25 tests) 6ms

 Test Files  2 passed (2)
      Tests  37 passed (37)
   Start at  05:58:15
   Duration  5.44s
```

このようにしていました。では静的コードの解析はどうやりましたか？フォーマットはどうやりましたか？調べるのも面倒ですよね。いちいち調べるのが面倒なことは全部 **タスクランナー** にやらせるようにしましょう。

> タスクランナーとは、アプリケーションのビルドなど、一定の手順で行う作業をコマンド一つで実行できるように予めタスクとして定義したものです。
> 
> —  Node.js実践入門 

TypeScriptの **タスクランナー** は `Gulp` です。

> GulpはJavaScript/TypeScriptにおけるタスクランナーです。gulpコマンドと起点となるgulpfile.jsというタスクを記述するファイルを用意することで、タスクの実行や登録されたタスクの一覧表示を行えます。
> 
> —  Gulp公式ドキュメント 

早速、テストタスクから作成しましょう。まず `gulpfile.js` を作ります。

```bash
$ touch gulpfile.js
```

```javascript
import { watch, series } from 'gulp'
import shell from 'gulp-shell'

// テストタスク
export const test = shell.task(['npm run test'])

// テストカバレッジタスク
export const coverage = shell.task(['npm run test:coverage'])

// 静的コード解析タスク
export const lint = shell.task(['npm run lint'])

// 自動修正付き静的コード解析タスク
export const lintFix = shell.task(['npm run lint:fix'])

// フォーマットタスク
export const format = shell.task(['npm run format'])

// フォーマットチェックタスク
export const formatCheck = shell.task(['npm run format:check'])

// ビルドタスク
export const build = shell.task(['npm run build'])

// 開発サーバータスク
export const dev = shell.task(['npm run dev'])

// 全体チェックタスク（自動修正付き）
export const checkAndFix = series(lintFix, format, test)

// ファイル監視タスク（Ruby入門2のGuardに対応）
export function guard() {
  console.log('🔍 Guard is watching for file changes...')
  console.log('Files will be automatically linted, formatted, and tested on change.')
  watch('src/**/*.ts', series(lintFix, format, test))
  watch('**/*.test.ts', series(test))
}

// ファイル監視タスク
export function watchFiles() {
  watch('src/**/*.ts', series(formatCheck, lint, test))
  watch('**/*.test.ts', series(test))
}

// デフォルトタスク（Ruby入門2のGuardのような自動化）
export default series(checkAndFix, guard)

// ウォッチタスクのエイリアス
export { watchFiles as watch }
```

タスクが登録されたか確認してみましょう。

```bash
$ npx gulp --tasks
[06:00:00] Tasks for ~/app/gulpfile.js
[06:00:00] ├── test
[06:00:00] ├── coverage
[06:00:00] ├── lint
[06:00:00] ├── lintFix
[06:00:00] ├── format
[06:00:00] ├── formatCheck
[06:00:00] ├── build
[06:00:00] ├── dev
[06:00:00] ├── checkAndFix
[06:00:00] ├── guard
[06:00:00] ├── watchFiles
[06:00:00] ├── watch
[06:00:00] └─┬ default
[06:00:00]   └─┬ <series>
[06:00:00]     ├─┬ checkAndFix
[06:00:00]     │ └─┬ <series>
[06:00:00]     │   ├── lintFix
[06:00:00]     │   ├── format
[06:00:00]     │   └── test
[06:00:00]     └── guard
```

タスクが登録されたことが確認できたのでタスクを実行します。

```bash
$ npx gulp test
[06:00:15] Using gulpfile ~/app/gulpfile.js
[06:00:15] Starting 'test'...

> app@0.0.0 test
> vitest run

 ✓ src/array-learning.test.ts (12 tests) 5ms
 ✓ src/fizz-buzz.test.ts (25 tests) 6ms

 Test Files  2 passed (2)
      Tests  37 passed (37)
   Start at  06:00:17
   Duration  5.16s

[06:00:22] Finished 'test' after 7.74 s
```

テストタスクが実行されたことが確認できたので引き続き静的コードの解析タスクを追加します。

タスクが登録されたことを確認します。

```bash
$ npx gulp --tasks
[06:00:30] Tasks for ~/app/gulpfile.js
[06:00:30] ├── lint
[06:00:30] ├── lintFix
```

続いてタスクを実行してみましょう。

```bash
$ npx gulp lint
[06:00:35] Using gulpfile ~/app/gulpfile.js
[06:00:35] Starting 'lint'...

> app@0.0.0 lint
> eslint . --ext .ts,.tsx

[06:00:37] Finished 'lint' after 2.1 s
```

うまく実行されたようですね。後、フォーマットコマンドもタスクとして追加しておきましょう。

```bash
$ npx gulp format
[06:00:42] Using gulpfile ~/app/gulpfile.js
[06:00:42] Starting 'format'...

> app@0.0.0 format
> prettier --write .

[06:00:43] Finished 'format' after 1.2 s
```

セットアップができたのでコミットしておきましょう。

```bash
$ git add .
$ git commit -m 'chore: タスクランナーセットアップ'
```

### タスクの自動化

良いコードを書くためのタスクをまとめることができました。でも、どうせなら自動で実行できるようにしたいですよね。タスクを自動実行するためのパッケージを追加します。[chokidar-cli](https://github.com/open-cli-tools/chokidar-cli) と [concurrently](https://github.com/open-cli-tools/concurrently) をインストールします。それぞれの詳細は以下を参照してください。

  - [TypeScript | chokidar-cli を利用してファイルの変更を検出し、任意のタスクを自動実行する](https://qiita.com/tbpgr/items/f5be21d8e19dd852d9b7)
  - [concurrentlyでソースコードの変更を監視して自動でbuild&実行させる](https://qiita.com/emergent/items/0a38909206844265e0b5)

```bash
$ npm install -D chokidar-cli concurrently
```

package.jsonに以下のスクリプトを追加します。

```json
{
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "c8 vitest run",
    "lint": "eslint . --ext .ts,.tsx",
    "lint:fix": "eslint . --ext .ts,.tsx --fix",
    "format": "prettier --write .",
    "format:check": "prettier --check .",
    "gulp": "gulp",
    "watch": "gulp watch",
    "guard": "gulp guard",
    "check": "gulp checkAndFix"
  }
}
```

`guard` が起動するか確認して一旦終了します。

```bash
$ npm run guard
> app@0.0.0 guard
> gulp guard

[06:01:00] Using gulpfile ~/app/gulpfile.js
[06:01:00] Starting 'guard'...
🔍 Guard is watching for file changes...
Files will be automatically linted, formatted, and tested on change.
[06:01:00] Finished 'guard' after 15 ms
# Ctrl+C で終了
```

続いて `package.json` にguardスクリプトを追加します。あと、guardスクリプトをsetupスクリプトに追加して初期セットアップ時に実行されるようにしておきます。

```json
{
  "scripts": {
    "dev": "vite",
    "build": "tsc && vite build",
    "preview": "vite preview",
    "test": "vitest run",
    "test:watch": "vitest",
    "test:coverage": "c8 vitest run",
    "lint": "eslint . --ext .ts,.tsx",
    "lint:fix": "eslint . --ext .ts,.tsx --fix",
    "format": "prettier --write .",
    "format:check": "prettier --check .",
    "gulp": "gulp",
    "watch": "gulp watch",
    "guard": "gulp guard",
    "check": "gulp checkAndFix",
    "setup": "npm install && npm run check"
  }
}
```

自動実行タスクを起動しましょう。

```bash
$ npm run guard
> app@0.0.0 guard
> gulp guard

[06:01:30] Using gulpfile ~/app/gulpfile.js
[06:01:30] Starting 'guard'...
🔍 Guard is watching for file changes...
Files will be automatically linted, formatted, and tested on change.
[06:01:30] Finished 'guard' after 15 ms
```

起動したら `fizz-buzz.test.ts` を編集してテストが自動実行されるか確認しましょう。

```typescript
// ...
describe('FizzBuzz', () => {
  describe('三の倍数の場合', () => {
    it('3を渡したら文字列Fizzを返す', () => {
      expect(FizzBuzz.generate(3)).toBe('FizzFizz') // わざとエラーにする
    })
  })
// ...
```

```bash
[06:01:45] Starting 'lintFix'...
> app@0.0.0 lint:fix
> eslint . --ext .ts,.tsx --fix
[06:01:47] Finished 'lintFix' after 2.1 s
[06:01:47] Starting 'format'...
> app@0.0.0 format
> prettier --write .
[06:01:48] Finished 'format' after 900 ms
[06:01:48] Starting 'test'...
> app@0.0.0 test
> vitest run

 FAIL  src/fizz-buzz.test.ts > FizzBuzz > 三の倍数の場合 > 3を渡したら文字列Fizzを返す
AssertionError: expected "Fizz" to be "FizzFizz"

 Test Files  1 failed | 1 passed (2)
      Tests  1 failed | 36 passed (37)
   Start at  06:01:50
   Duration  3.8s

[06:01:54] 'test' errored after 6.2 s
```

変更を感知してテストが実行されてた結果失敗していました。コードを元に戻してテストをパスするようにしておきましょう。テストがパスすることが確認できたらコミットしておきましょう。このときターミナルでは `guard` が動いているので別ターミナルを開いてコミットを実施すると良いでしょう。

```bash
$ git add .
$ git commit -m 'chore: タスクの自動化'
```

これで [ソフトウェア開発の三種の神器](https://t-wada.hatenablog.jp/entry/clean-code-that-works) の最後のアイテムの準備ができました。次回の開発からは最初にコマンドラインで `npm run guard` を実行すれば良いコードを書くためのタスクを自動でやってくるようになるのでコードを書くことに集中できるようになりました。では、次のエピソードに進むとしましょう。
