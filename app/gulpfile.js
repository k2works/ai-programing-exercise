/**
 * Gulpfile for ClojureScript FizzBuzz Application
 * テスト駆動開発から始めるClojure入門 タスクランナー
 */

const { src, dest, watch, series, parallel, task } = require('gulp');
const { exec } = require('child_process');
const path = require('path');

// ヘルプタスク
function help(cb) {
  console.log('\n使用可能なタスク:');
  console.log('  gulp help        - このヘルプを表示');
  console.log('  gulp setup       - 依存関係のセットアップ');
  console.log('  gulp test        - テストを実行');
  console.log('  gulp build       - アプリケーションをビルド');
  console.log('  gulp watch       - 開発モードでwatch');
  console.log('  gulp release     - リリースビルド');
  console.log('  gulp server      - shadow-cljsサーバーを起動');
  console.log('  gulp clean       - 生成されたファイルをクリーンアップ');
  console.log('  gulp dev         - 開発環境セットアップ（server + watch）');
  console.log('  gulp check       - 全ての品質チェックを実行');
  console.log('  gulp format      - コードフォーマットをチェック');
  console.log('  gulp format-fix  - コードフォーマットを自動修正');
  console.log('  gulp lint        - 静的コード解析を実行');
  console.log('  gulp coverage    - コードカバレッジを実行');
  console.log('');
  cb();
}

// 依存関係のセットアップ
function setup(cb) {
  console.log('依存関係を確認中...');
  exec('npm install', (err, stdout, stderr) => {
    if (err) {
      console.error('依存関係のインストールでエラーが発生しました:', err);
      return cb(err);
    }
    console.log(stdout);
    if (stderr) console.error(stderr);
    console.log('依存関係のセットアップが完了しました。');
    cb();
  });
}

// テスト実行
function test(cb) {
  console.log('テストを実行中...');
  exec('npx shadow-cljs compile test && node out/test.js', (err, stdout, stderr) => {
    if (err) {
      console.error('テスト実行でエラーが発生しました:', err);
      return cb(err);
    }
    console.log(stdout);
    if (stderr) console.error(stderr);
    console.log('テストが完了しました。');
    cb();
  });
}

// ビルド
function build(cb) {
  console.log('アプリケーションをビルド中...');
  exec('npx shadow-cljs compile app', (err, stdout, stderr) => {
    if (err) {
      console.error('ビルドでエラーが発生しました:', err);
      return cb(err);
    }
    console.log(stdout);
    if (stderr) console.error(stderr);
    console.log('ビルドが完了しました。');
    cb();
  });
}

// 開発モードでwatch
function watchDev(cb) {
  console.log('開発モードでwatchを開始中...');
  const child = exec('npx shadow-cljs watch app', (err, stdout, stderr) => {
    if (err && err.signal !== 'SIGTERM') {
      console.error('Watchでエラーが発生しました:', err);
      return cb(err);
    }
  });
  
  child.stdout.on('data', (data) => {
    console.log(data);
  });
  
  child.stderr.on('data', (data) => {
    console.error(data);
  });
  
  // プロセスを継続するためにcbは呼ばない
}

// リリースビルド
function release(cb) {
  console.log('リリースビルドを実行中...');
  exec('npx shadow-cljs release app', (err, stdout, stderr) => {
    if (err) {
      console.error('リリースビルドでエラーが発生しました:', err);
      return cb(err);
    }
    console.log(stdout);
    if (stderr) console.error(stderr);
    console.log('リリースビルドが完了しました。');
    cb();
  });
}

// shadow-cljsサーバー起動
function server(cb) {
  console.log('shadow-cljsサーバーを起動中...');
  const child = exec('npx shadow-cljs server', (err, stdout, stderr) => {
    if (err && err.signal !== 'SIGTERM') {
      console.error('サーバー起動でエラーが発生しました:', err);
      return cb(err);
    }
  });
  
  child.stdout.on('data', (data) => {
    console.log(data);
  });
  
  child.stderr.on('data', (data) => {
    console.error(data);
  });
  
  console.log('shadow-cljsサーバーが起動しました。http://localhost:9630 でアクセスできます。');
  // プロセスを継続するためにcbは呼ばない
}

// クリーンアップ
function clean(cb) {
  console.log('クリーンアップ中...');
  exec('rm -rf public/js/* out/ .shadow-cljs/ target/', (err, stdout, stderr) => {
    if (err) {
      console.error('クリーンアップでエラーが発生しました:', err);
      return cb(err);
    }
    console.log('クリーンアップが完了しました。');
    cb();
  });
}

// 静的コード解析（ClojureScript用に調整）
function lint(cb) {
  console.log('静的コード解析を実行中...');
  // ClojureScriptの場合、clj-kondoを使用することが多い
  // ここではshadow-cljsのコンパイルエラーチェックを代用
  exec('npx shadow-cljs compile app --verbose', (err, stdout, stderr) => {
    if (err) {
      console.error('静的コード解析でエラーが発見されました:', err);
      return cb(err);
    }
    console.log('静的コード解析が完了しました。問題は見つかりませんでした。');
    cb();
  });
}

// コードフォーマットチェック（cljfmtがあれば使用）
function format(cb) {
  console.log('コードフォーマットをチェック中...');
  // cljfmtがインストールされていれば使用
  exec('which cljfmt', (err, stdout, stderr) => {
    if (err) {
      console.log('cljfmtが見つかりません。フォーマットチェックをスキップします。');
      return cb();
    }
    
    exec('cljfmt check src/ test/', (err, stdout, stderr) => {
      if (err) {
        console.error('フォーマットエラーが見つかりました:', stderr);
        return cb(err);
      }
      console.log('コードフォーマットは正常です。');
      cb();
    });
  });
}

// コードフォーマット自動修正
function formatFix(cb) {
  console.log('コードフォーマットを自動修正中...');
  exec('which cljfmt', (err, stdout, stderr) => {
    if (err) {
      console.log('cljfmtが見つかりません。フォーマット修正をスキップします。');
      return cb();
    }
    
    exec('cljfmt fix src/ test/', (err, stdout, stderr) => {
      if (err) {
        console.error('フォーマット修正でエラーが発生しました:', err);
        return cb(err);
      }
      console.log('コードフォーマットの自動修正が完了しました。');
      cb();
    });
  });
}

// コードカバレッジ（ClojureScript用の設定が必要）
function coverage(cb) {
  console.log('コードカバレッジを実行中...');
  // ClojureScriptのカバレッジツールは限定的なので、テスト実行で代用
  console.log('注意: ClojureScriptのコードカバレッジは制限があります。');
  console.log('テスト結果を参考にしてください。');
  exec('npx shadow-cljs compile test && node out/test.js', (err, stdout, stderr) => {
    if (err) {
      console.error('カバレッジ測定でエラーが発生しました:', err);
      return cb(err);
    }
    console.log(stdout);
    console.log('カバレッジ測定が完了しました。');
    cb();
  });
}

// 開発環境セットアップ（サーバー + watch）
const dev = parallel(server, watchDev);

// 全体の品質チェック
const check = series(lint, format, test);

// タスクの登録
exports.help = help;
exports.setup = setup;
exports.test = test;
exports.build = build;
exports.watch = watchDev;
exports.release = release;
exports.server = server;
exports.clean = clean;
exports.lint = lint;
exports.format = format;
exports['format-fix'] = formatFix;
exports.coverage = coverage;
exports.dev = dev;
exports.check = check;

// デフォルトタスク
exports.default = help;
